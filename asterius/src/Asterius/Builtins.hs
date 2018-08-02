{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , getDefaultBuiltinsOptions
  , unsafeDefaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , rtsAsteriusFunctionImports
  , rtsAsteriusFunctionExports
  , marshalErrorCode
  , errGCEnter1
  , errGCFun
  , errBarf
  , errStgGC
  , errUnreachableBlock
  , errHeapOverflow
  , errMegaBlockGroup
  , errUnimplemented
  , errAtomics
  , errSetBaseReg
  , errBrokenFunction
  , errAssert
  , wasmPageSize
  , cutI64
  , generateWasmFunctionTypeName
  ) where

import Asterius.BuildInfo
import Asterius.EDSL
import Asterius.Internals
import Asterius.Types
import Asterius.TypesConv
import Control.Monad
import qualified Data.ByteString.Short as SBS
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import Foreign
import qualified GHC
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Constants
import Prelude hiding (IO)
import System.IO.Unsafe

wasmPageSize :: Int
wasmPageSize = 65536

data BuiltinsOptions = BuiltinsOptions
  { dflags :: GHC.DynFlags
  , nurseryGroups, threadStateSize :: Int
  , tracing :: Bool
  }

getDefaultBuiltinsOptions :: IO BuiltinsOptions
getDefaultBuiltinsOptions =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
  GHC.runGhc (Just ghcLibDir) $ do
    _ <- GHC.getSessionDynFlags >>= GHC.setSessionDynFlags
    dflags <- GHC.getSessionDynFlags
    pure
      BuiltinsOptions
        { dflags = dflags
        , nurseryGroups = blocks_per_mblock * 1024
        , threadStateSize = 65536
        , tracing = False
        }

{-# NOINLINE unsafeDefaultBuiltinsOptions #-}
unsafeDefaultBuiltinsOptions :: BuiltinsOptions
unsafeDefaultBuiltinsOptions = unsafePerformIO getDefaultBuiltinsOptions

rtsAsteriusModuleSymbol :: AsteriusModuleSymbol
rtsAsteriusModuleSymbol =
  AsteriusModuleSymbol
    { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS GHC.rtsUnitId
    , moduleName = ["Asterius"]
    }

rtsAsteriusModule :: BuiltinsOptions -> AsteriusModule
rtsAsteriusModule opts =
  mempty
    { staticsMap =
        [ ("g0", AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        , ( "blocked_queue_hd"
          , AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        , ( "blocked_queue_tl"
          , AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        , ( "enabled_capabilities"
          , AsteriusStatics {asteriusStatics = [Uninitialized 4]})
        , ( "large_alloc_lim"
          , AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        , ( "MainCapability"
          , AsteriusStatics
              { asteriusStatics =
                  [ Serialized $
                    SBS.pack $
                    replicate (8 * roundup_bytes_to_words sizeof_Capability) 0
                  ]
              })
        , ( "n_capabilities"
          , AsteriusStatics {asteriusStatics = [Uninitialized 4]})
        , ( "rts_stop_on_exception"
          , AsteriusStatics {asteriusStatics = [Uninitialized 4]})
        , ( "RtsFlags"
          , AsteriusStatics
              { asteriusStatics =
                  [Uninitialized (8 * roundup_bytes_to_words sizeof_RTS_FLAGS)]
              })
        , ( "stable_ptr_table"
          , AsteriusStatics {asteriusStatics = [Uninitialized 8]})
        ]
    , functionMap =
        [ ("main", mainFunction opts)
        , ("hs_init", hsInitFunction opts)
        , ("rts_evalLazyIO", rtsEvalLazyIOFunction opts)
        , ("setTSOLink", setTSOLinkFunction opts)
        , ("setTSOPrev", setTSOPrevFunction opts)
        , ("scheduleWaitThread", scheduleWaitThreadFunction opts)
        , ("createThread", createThreadFunction opts)
        , ("createIOThread", createIOThreadFunction opts)
        , ("allocate", allocateFunction opts)
        , ("allocGroupOnNode", allocGroupOnNodeFunction opts)
        , ("getMBlocks", getMBlocksFunction opts)
        , ("free", freeFunction opts)
        , ("newCAF", newCAFFunction opts)
        , ("StgRun", stgRunFunction opts)
        , ("StgReturn", stgReturnFunction opts)
        , ("print_i64", printI64Function opts)
        , ("print_f32", printF32Function opts)
        , ("print_f64", printF64Function opts)
        , ("__asterius_Load_Sp", getI32GlobalRegFunction opts Sp)
        , ("__asterius_Load_SpLim", getI32GlobalRegFunction opts SpLim)
        , ("__asterius_Load_Hp", getI32GlobalRegFunction opts Hp)
        , ("__asterius_Load_HpLim", getI32GlobalRegFunction opts HpLim)
        , ("__asterius_memory_trap", memoryTrapFunction opts)
        , ("__asterius_Load_I8", loadWrapperFunction opts 1 I32)
        , ("__asterius_Load_I16", loadWrapperFunction opts 2 I32)
        , ("__asterius_Load_I32", loadWrapperFunction opts 4 I32)
        , ("__asterius_Load_I64", loadWrapperFunction opts 8 I64)
        , ("__asterius_Load_F32", loadWrapperFunction opts 4 F32)
        , ("__asterius_Load_F64", loadWrapperFunction opts 8 F64)
        , ("__asterius_Store_I8", storeWrapperFunction opts 1 I32)
        , ("__asterius_Store_I16", storeWrapperFunction opts 2 I32)
        , ("__asterius_Store_I32", storeWrapperFunction opts 4 I32)
        , ("__asterius_Store_I64", storeWrapperFunction opts 8 I64)
        , ("__asterius_Store_F32", storeWrapperFunction opts 4 F32)
        , ("__asterius_Store_F64", storeWrapperFunction opts 8 F64)
        ]
    }

rtsAsteriusFunctionImports :: Bool -> [AsteriusFunctionImport]
rtsAsteriusFunctionImports debug =
  [ AsteriusFunctionImport
    { internalName = "__asterius_" <> op <> "_" <> showSBS ft
    , externalModuleName = "Math"
    , externalBaseName = op
    , functionType = FunctionType {returnType = ft, paramTypes = [ft]}
    }
  | ft <- [F32, F64]
  , op <-
      [ "sin"
      , "cos"
      , "tan"
      , "sinh"
      , "cosh"
      , "tanh"
      , "asin"
      , "acos"
      , "atan"
      , "log"
      , "exp"
      ]
  ] <>
  [ AsteriusFunctionImport
    { internalName = "__asterius_" <> op <> "_" <> showSBS ft
    , externalModuleName = "Math"
    , externalBaseName = op
    , functionType = FunctionType {returnType = ft, paramTypes = [ft, ft]}
    }
  | ft <- [F32, F64]
  , op <- ["pow"]
  ] <>
  [ AsteriusFunctionImport
      { internalName = "printI64"
      , externalModuleName = "rts"
      , externalBaseName = "printI64"
      , functionType = FunctionType {returnType = None, paramTypes = [I32, I32]}
      }
  , AsteriusFunctionImport
      { internalName = "printF32"
      , externalModuleName = "rts"
      , externalBaseName = "print"
      , functionType = FunctionType {returnType = None, paramTypes = [F32]}
      }
  , AsteriusFunctionImport
      { internalName = "printF64"
      , externalModuleName = "rts"
      , externalBaseName = "print"
      , functionType = FunctionType {returnType = None, paramTypes = [F64]}
      }
  , AsteriusFunctionImport
      { internalName = "__asterius_errorI32"
      , externalModuleName = "rts"
      , externalBaseName = "panic"
      , functionType = FunctionType {returnType = None, paramTypes = [I32]}
      }
  ] <>
  (if debug
     then [ AsteriusFunctionImport
              { internalName = "__asterius_traceCmm"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmm"
              , functionType =
                  FunctionType {returnType = None, paramTypes = [I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_traceCmmBlock"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmmBlock"
              , functionType =
                  FunctionType {returnType = None, paramTypes = [I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_traceCmmSetLocal"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_traceCmmSetLocal"
              , functionType =
                  FunctionType
                    {returnType = None, paramTypes = [I32, I32, I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_current_memory"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_current_memory"
              , functionType =
                  FunctionType {returnType = I32, paramTypes = [I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_grow_memory"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_grow_memory"
              , functionType =
                  FunctionType {returnType = I32, paramTypes = [I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_memory_trap_trigger"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_memory_trap_trigger"
              , functionType =
                  FunctionType {returnType = None, paramTypes = [I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_load_i64"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_load_i64"
              , functionType =
                  FunctionType
                    {returnType = None, paramTypes = [I32, I32, I32, I32]}
              }
          , AsteriusFunctionImport
              { internalName = "__asterius_store_i64"
              , externalModuleName = "rts"
              , externalBaseName = "__asterius_store_i64"
              , functionType =
                  FunctionType
                    {returnType = None, paramTypes = [I32, I32, I32, I32]}
              }
          ] <>
          concat
            [ [ AsteriusFunctionImport
                  { internalName = "__asterius_load_" <> k
                  , externalModuleName = "rts"
                  , externalBaseName = "__asterius_load_" <> k
                  , functionType =
                      FunctionType
                        {returnType = None, paramTypes = [I32, I32, t]}
                  }
              , AsteriusFunctionImport
                  { internalName = "__asterius_store_" <> k
                  , externalModuleName = "rts"
                  , externalBaseName = "__asterius_store_" <> k
                  , functionType =
                      FunctionType
                        {returnType = None, paramTypes = [I32, I32, t]}
                  }
            ]
            | (k, t) <-
                [ ("i8", I32)
                , ("i16", I32)
                , ("i32", I32)
                , ("f32", F32)
                , ("f64", F64)
                ]
            ]
     else [])

rtsAsteriusFunctionExports :: Bool -> V.Vector FunctionExport
rtsAsteriusFunctionExports debug =
  V.fromList
    [ FunctionExport {internalName = f, externalName = f}
    | f <-
        if debug
          then [ "hs_init"
               , "main"
               , "__asterius_Load_Sp"
               , "__asterius_Load_SpLim"
               , "__asterius_Load_Hp"
               , "__asterius_Load_HpLim"
               ]
          else ["hs_init", "main"]
    ]

{-# INLINEABLE marshalErrorCode #-}
marshalErrorCode :: Int32 -> ValueType -> Expression
marshalErrorCode err vt =
  Block
    { name = ""
    , bodys =
        [ CallImport
            { target' = "__asterius_errorI32"
            , operands = [ConstI32 err]
            , valueType = None
            }
        , Unreachable
        ]
    , valueType = vt
    }

errGCEnter1, errGCFun, errBarf, errStgGC, errUnreachableBlock, errHeapOverflow, errMegaBlockGroup, errUnimplemented, errAtomics, errSetBaseReg, errBrokenFunction, errAssert ::
     Int32
errGCEnter1 = 1

errGCFun = 2

errBarf = 3

errStgGC = 4

errUnreachableBlock = 5

errHeapOverflow = 6

errMegaBlockGroup = 7

errUnimplemented = 8

errAtomics = 9

errSetBaseReg = 10

errBrokenFunction = 11

errAssert = 12

assert :: BuiltinsOptions -> Expression -> EDSL ()
assert BuiltinsOptions {..} cond =
  when tracing $ if' cond mempty $ emit $ marshalErrorCode errAssert None

mainFunction, hsInitFunction, rtsEvalLazyIOFunction, setTSOLinkFunction, setTSOPrevFunction, scheduleWaitThreadFunction, createThreadFunction, createIOThreadFunction, allocateFunction, allocGroupOnNodeFunction, getMBlocksFunction, freeFunction, newCAFFunction, stgRunFunction, stgReturnFunction, printI64Function, printF32Function, printF64Function, memoryTrapFunction ::
     BuiltinsOptions -> AsteriusFunction
mainFunction BuiltinsOptions {..} =
  runEDSL $
  call "rts_evalLazyIO" [mainCapability, symbol "Main_main_closure", constI64 0]

initCapability :: Expression -> Expression -> EDSL ()
initCapability cap i = do
  storeI32 cap offset_Capability_no i
  storeI32 cap offset_Capability_node $ constI32 0
  storeI8 cap offset_Capability_in_haskell $ constI32 0
  storeI32 cap offset_Capability_idle $ constI32 0
  storeI8 cap offset_Capability_disabled $ constI32 0
  storeI64 cap offset_Capability_run_queue_hd endTSOQueue
  storeI64 cap offset_Capability_run_queue_tl endTSOQueue
  storeI32 cap offset_Capability_n_run_queue $ constI32 0
  storeI64 cap offset_Capability_total_allocated $ constI64 0
  storeI64 cap (offset_Capability_f + offset_StgFunTable_stgEagerBlackholeInfo) $
    symbol "__stg_EAGER_BLACKHOLE_info"
  storeI64 cap (offset_Capability_f + offset_StgFunTable_stgGCEnter1) $
    symbol "__stg_gc_enter_1"
  storeI64 cap (offset_Capability_f + offset_StgFunTable_stgGCFun) $
    symbol "__stg_gc_fun"
  storeI64 cap offset_Capability_weak_ptr_list_hd $ constI64 0
  storeI64 cap offset_Capability_weak_ptr_list_tl $ constI64 0
  storeI64 cap offset_Capability_free_tvar_watch_queues $
    symbol "stg_END_STM_WATCH_QUEUE_closure"
  storeI64 cap offset_Capability_free_trec_chunks $
    symbol "stg_END_STM_CHUNK_LIST_closure"
  storeI64 cap offset_Capability_free_trec_headers $
    symbol "stg_NO_TREC_closure"
  storeI32 cap offset_Capability_transaction_tokens $ constI32 0
  storeI32 cap offset_Capability_context_switch $ constI32 0
  storeI64 cap offset_Capability_pinned_object_block $ constI64 0
  storeI64 cap offset_Capability_pinned_object_blocks $ constI64 0
  storeI64 cap (offset_Capability_r + offset_StgRegTable_rCCCS) $ constI64 0
  storeI64 cap (offset_Capability_r + offset_StgRegTable_rCurrentTSO) $
    constI64 0

hsInitFunction BuiltinsOptions {..} =
  runEDSL $ do
    initCapability mainCapability (constI32 0)
    bd <- call' "allocGroupOnNode" [constI32 0, constI64 nurseryGroups] I64
    putLVal hp $ loadI64 bd offset_bdescr_start
    putLVal hpLim $
      getLVal hp `addInt64`
      (extendUInt32 (loadI32 bd offset_bdescr_blocks) `mulInt64`
       constI64 block_size)
    putLVal cccs (constI64 0)
    putLVal currentNursery bd
    storeI64 (getLVal baseReg) offset_StgRegTable_rCurrentAlloc bd
    task <-
      call'
        "allocate"
        [mainCapability, constI64 $ roundup_bytes_to_words sizeof_Task]
        I64
    incall <-
      call'
        "allocate"
        [mainCapability, constI64 $ roundup_bytes_to_words sizeof_InCall]
        I64
    storeI64 mainCapability offset_Capability_running_task task
    storeI64 task offset_Task_cap mainCapability
    storeI64 task offset_Task_incall incall
    storeI64 incall offset_InCall_task task

rtsEvalLazyIOFunction BuiltinsOptions {..} =
  runEDSL $ do
    [cap, p, ret] <- params [I64, I64, I64]
    tso <-
      call'
        "createIOThread"
        [cap, constI64 $ roundup_bytes_to_words threadStateSize, p]
        I64
    call "scheduleWaitThread" [tso, ret, cap]

appendToRunQueue :: BuiltinsOptions -> Expression -> Expression -> EDSL ()
appendToRunQueue opts cap tso = do
  assert opts $ loadI64 tso offset_StgTSO__link `eqInt64` endTSOQueue
  if'
    (loadI64 cap offset_Capability_run_queue_hd `eqInt64` endTSOQueue)
    (do storeI64 cap offset_Capability_run_queue_hd tso
        storeI64
          tso
          (offset_StgTSO_block_info + offset_StgTSOBlockInfo_prev)
          endTSOQueue)
    (do call "setTSOLink" [cap, loadI64 cap offset_Capability_run_queue_tl, tso]
        call "setTSOPrev" [cap, tso, loadI64 cap offset_Capability_run_queue_tl])
  storeI64 cap offset_Capability_run_queue_tl tso
  storeI32 cap offset_Capability_n_run_queue $
    loadI32 cap offset_Capability_n_run_queue `addInt32` constI32 1

setTSOLinkFunction _ =
  runEDSL $ do
    [_, tso, target] <- params [I64, I64, I64]
    if'
      (eqZInt32 (loadI32 tso offset_StgTSO_dirty))
      (storeI32 tso offset_StgTSO_dirty (constI32 1))
      mempty
    storeI64 tso offset_StgTSO__link target

setTSOPrevFunction _ =
  runEDSL $ do
    [_, tso, target] <- params [I64, I64, I64]
    if'
      (eqZInt32 (loadI32 tso offset_StgTSO_dirty))
      (storeI32 tso offset_StgTSO_dirty (constI32 1))
      mempty
    storeI64 tso (offset_StgTSO_block_info + offset_StgTSOBlockInfo_prev) target

schedule :: Expression -> Expression -> EDSL ()
schedule = undefined

scheduleWaitThreadFunction' opts =
  runEDSL $ do
    [tso, ret, cap] <- params [I64, I64, I64]
    task <- i64Local $ loadI64 cap offset_Capability_running_task
    storeI64 tso offset_StgTSO_bound $ loadI64 task offset_Task_incall
    storeI64 tso offset_StgTSO_cap cap
    incall <- i64Local $ loadI64 task offset_Task_incall
    storeI64 incall offset_InCall_tso tso
    storeI64 incall offset_InCall_ret ret
    storeI32 incall offset_InCall_rstat $ constI32 scheduler_NoStatus
    appendToRunQueue opts cap tso
    schedule cap task
    assert opts $
      loadI32 (loadI64 task offset_Task_incall) offset_InCall_rstat `neInt32`
      constI32 scheduler_NoStatus

scheduleWaitThreadFunction _ =
  runEDSL $ do
    [tso, ret, cap] <- params [I64, I64, I64]
    task <- i64Local $ loadI64 cap offset_Capability_running_task
    storeI64 tso offset_StgTSO_bound $ loadI64 task offset_Task_incall
    storeI64 tso offset_StgTSO_cap cap
    incall <- i64Local $ loadI64 task offset_Task_incall
    storeI64 incall offset_InCall_tso tso
    storeI64 incall offset_InCall_ret ret
    storeI64 cap (offset_Capability_r + offset_StgRegTable_rCurrentTSO) tso
    storeI32 cap offset_Capability_interrupt (constI32 0)
    _ <-
      call'
        "StgRun"
        [ symbol "stg_returnToStackTop"
        , cap `addInt64` constI64 offset_Capability_r
        ]
        I64
    storeI64 incall offset_InCall_ret $
      loadI64 (loadI64 tso $ offset_StgTSO_StgStack + offset_StgStack_sp) 8

createThreadFunction _ =
  runEDSL $ do
    setReturnType I64
    [cap, alloc_words] <- params [I64, I64]
    tso_p <- call' "allocate" [cap, alloc_words] I64
    putLVal currentTSO tso_p
    stack_p <- i64Local $ tso_p `addInt64` constI64 offset_StgTSO_StgStack
    storeI64 stack_p 0 $ symbol "stg_STACK_info"
    stack_size_w <-
      i64Local $
      alloc_words `subInt64`
      constI64 ((offset_StgTSO_StgStack + offset_StgStack_stack) `div` 8)
    storeI32 stack_p offset_StgStack_stack_size $ wrapInt64 stack_size_w
    storeI64 stack_p offset_StgStack_sp $
      (stack_p `addInt64` constI64 offset_StgStack_stack) `addInt64`
      stack_size_w
    storeI32 stack_p offset_StgStack_dirty $ constI32 1
    storeI64 tso_p 0 $ symbol "stg_TSO_info"
    storeI16 tso_p offset_StgTSO_what_next $ constI32 next_ThreadRunGHC
    storeI16 tso_p offset_StgTSO_why_blocked $ constI32 blocked_NotBlocked
    storeI64
      tso_p
      (offset_StgTSO_block_info + offset_StgTSOBlockInfo_closure)
      endTSOQueue
    storeI64 tso_p offset_StgTSO_blocked_exceptions endTSOQueue
    storeI64 tso_p offset_StgTSO_bq endTSOQueue
    storeI32 tso_p offset_StgTSO_flags $ constI32 0
    storeI32 tso_p offset_StgTSO_dirty $ constI32 1
    storeI64 tso_p offset_StgTSO__link endTSOQueue
    storeI32 tso_p offset_StgTSO_saved_errno $ constI32 0
    storeI64 tso_p offset_StgTSO_bound $ constI64 0
    storeI64 tso_p offset_StgTSO_cap cap
    storeI64 tso_p offset_StgTSO_stackobj stack_p
    storeI32 tso_p offset_StgTSO_tot_stack_size $ wrapInt64 stack_size_w
    storeI64 tso_p offset_StgTSO_alloc_limit (constI64 0)
    storeI64 tso_p offset_StgTSO_trec $ symbol "stg_NO_TREC_closure"
    storeI64 stack_p offset_StgStack_sp $
      loadI64 stack_p offset_StgStack_sp `subInt64`
      constI64 (8 * roundup_bytes_to_words sizeof_StgStopFrame)
    storeI64 (loadI64 stack_p offset_StgStack_sp) 0 $
      symbol "stg_stop_thread_info"
    emit tso_p

pushClosure :: Expression -> Expression -> EDSL ()
pushClosure tso c = do
  stack_p <- i64Local $ loadI64 tso offset_StgTSO_stackobj
  storeI64 stack_p offset_StgStack_sp $
    loadI64 stack_p offset_StgStack_sp `subInt64` constI64 8
  storeI64 (loadI64 stack_p offset_StgStack_sp) 0 c

createIOThreadFunction _ =
  runEDSL $ do
    setReturnType I64
    [cap, stack_size, closure] <- params [I64, I64, I64]
    t <- call' "createThread" [cap, stack_size] I64
    pushClosure t $ symbol "stg_ap_v_info"
    pushClosure t closure
    pushClosure t $ symbol "stg_enter_info"
    emit t

allocateFunction _ =
  runEDSL $ do
    setReturnType I64
    [_, n] <- params [I64, I64]
    new_hp <- i64Local $ getLVal hp `addInt64` (n `mulInt64` constI64 8)
    if'
      (new_hp `gtUInt64` getLVal hpLim)
      (emit $ marshalErrorCode errHeapOverflow None)
      mempty
    old_hp <- i64Local $ getLVal hp
    putLVal hp new_hp
    storeI64
      (loadI64 (getLVal baseReg) offset_StgRegTable_rCurrentAlloc)
      offset_bdescr_free
      new_hp
    emit old_hp

blocksToMBlocks :: Expression -> EDSL Expression
blocksToMBlocks n = do
  r <- i64MutLocal
  if'
    (n `leUInt64` constI64 blocks_per_mblock)
    (putLVal r (constI64 1))
    (putLVal
       r
       (constI64 1 `addInt64`
        ((n `mulInt64` constI64 block_size) `divUInt64` constI64 mblock_size)))
  pure $ getLVal r

initGroup :: Expression -> EDSL ()
initGroup hd = do
  storeI64 hd offset_bdescr_free $ loadI64 hd offset_bdescr_start
  storeI64 hd offset_bdescr_link $ constI64 0

allocGroupOnNodeFunction _ =
  runEDSL $ do
    setReturnType I64
    [node, n] <- params [I32, I64]
    mblocks <- blocksToMBlocks n
    bd <- allocMegaGroup node mblocks
    initGroup bd
    emit bd

getMBlocksFunction _ =
  runEDSL $ do
    setReturnType I64
    n <- param I32
    ret <-
      i64Local $
      extendUInt32 $
      growMemory (n `mulInt32` constI32 (mblock_size `div` wasmPageSize)) `mulInt32`
      constI32 wasmPageSize
    emit ret

mblockGroupBlocks :: Expression -> Expression
mblockGroupBlocks n =
  constI64 blocks_per_mblock `addInt64`
  ((n `subInt64` constI64 1) `mulInt64` constI64 (mblock_size `div` block_size))

initMBlock :: Expression -> Expression -> EDSL ()
initMBlock mblock node = do
  block <- i64MutLocal
  putLVal block $ mblock `addInt64` constI64 offset_first_block
  bd <- i64MutLocal
  putLVal bd $ mblock `addInt64` constI64 offset_first_bdescr
  last_block <- i64Local $ mblock `addInt64` constI64 offset_last_block
  whileLoop (getLVal block `leUInt64` last_block) $ do
    storeI64 (getLVal bd) offset_bdescr_start (getLVal block)
    storeI16 (getLVal bd) offset_bdescr_node node
    putLVal bd $ getLVal bd `addInt64` constI64 sizeof_bdescr
    putLVal block $ getLVal block `addInt64` constI64 block_size

allocMegaGroup :: Expression -> Expression -> EDSL Expression
allocMegaGroup node mblocks = do
  mblock <- call' "getMBlocks" [wrapInt64 mblocks] I64
  initMBlock mblock node
  bd <- i64Local $ mblock `addInt64` constI64 offset_first_bdescr
  storeI32 bd offset_bdescr_blocks $ wrapI64 $ mblockGroupBlocks mblocks
  pure bd

freeFunction _ =
  runEDSL $ do
    _ <- param I64
    emit $ marshalErrorCode errUnimplemented None

newCAFFunction _ =
  runEDSL $ do
    setReturnType I64
    [reg, caf] <- params [I64, I64]
    orig_info <- i64Local $ loadI64 caf 0
    storeI64 caf offset_StgIndStatic_saved_info orig_info
    bh <-
      call'
        "allocate"
        [mainCapability, constI64 $ roundup_bytes_to_words sizeof_StgInd]
        I64
    storeI64 bh 0 $ symbol "stg_CAF_BLACKHOLE_info"
    storeI64 bh offset_StgInd_indirectee $
      loadI64 reg offset_StgRegTable_rCurrentTSO
    storeI64 caf offset_StgIndStatic_indirectee bh
    storeI64 caf 0 $ symbol "stg_IND_STATIC_info"
    emit bh

stgRunFunction BuiltinsOptions {..} =
  runEDSL $ do
    setReturnType I64
    f <- mutParam I64
    _ <- param I64
    loop' $ \loop_lbl ->
      if' (eqZInt64 (getLVal f)) mempty $ do
        f' <-
          callIndirect'
            (getLVal f `subInt64` constI64 1)
            []
            (FunctionType I64 [])
        putLVal f f'
        break' loop_lbl Null
    emit $ getLVal r1

stgReturnFunction _ =
  runEDSL $ do
    setReturnType I64
    emit $ constI64 0

printI64Function _ =
  runEDSL $ do
    x <- param I64
    callImport "printI64" (V.toList (cutI64 x))

printF32Function _ =
  runEDSL $ do
    x <- param F32
    callImport "printF32" [x]

printF64Function _ =
  runEDSL $ do
    x <- param F64
    callImport "printF64" [x]

getI32GlobalRegFunction ::
     BuiltinsOptions -> UnresolvedGlobalReg -> AsteriusFunction
getI32GlobalRegFunction _ gr =
  runEDSL $ do
    setReturnType I32
    emit $ wrapInt64 $ getLVal $ global gr

memoryTrapFunction _ =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = [I64]}
    , body =
        If
          { condition =
              V.foldl1' (Binary OrInt32) $
              V.fromList $
              [ guard_struct (ConstI64 0) 8 []
              , (task_p `neInt64` constI64 0) `andInt32`
                guard_struct
                  task_p
                  sizeof_Task
                  [offset_Task_cap, offset_Task_incall]
              , Binary
                  { binaryOp = AndInt32
                  , operand0 =
                      notExpr $ Unary {unaryOp = EqZInt64, operand0 = tso_p}
                  , operand1 =
                      guard_struct
                        tso_p
                        sizeof_StgTSO
                        [ 0
                        , offset_StgTSO_alloc_limit
                        , offset_StgTSO_blocked_exceptions
                        , offset_StgTSO_block_info
                        , offset_StgTSO_bound
                        , offset_StgTSO_bq
                        , offset_StgTSO_bound
                        , offset_StgTSO_cap
                        , offset_StgTSO_dirty
                        , offset_StgTSO_flags
                        , offset_StgTSO_saved_errno
                        , offset_StgTSO_stackobj
                        , offset_StgTSO_tot_stack_size
                        , offset_StgTSO_trec
                        , offset_StgTSO_what_next
                        , offset_StgTSO_why_blocked
                        , offset_StgTSO__link
                        ]
                  }
              ] <>
              [ guard_struct Unresolved {unresolvedSymbol = sym} size []
              | (sym, size) <-
                  [ ("g0", 8)
                  , ("blocked_queue_hd", 8)
                  , ("blocked_queue_tl", 8)
                  , ("enabled_capabilities", 4)
                  , ("large_alloc_lim", 8)
                  , ("n_capabilities", 4)
                  , ("rts_stop_on_exception", 4)
                  , ("RtsFlags", 8 * roundup_bytes_to_words sizeof_RTS_FLAGS)
                  , ("stable_ptr_table", 8)
                  ]
              ]
          , ifTrue =
              Block
                { name = ""
                , bodys =
                    [ CallImport
                        { target' = "__asterius_memory_trap_trigger"
                        , operands = cutI64 p
                        , valueType = None
                        }
                    , Unreachable
                    ]
                , valueType = None
                }
          , ifFalse = Null
          }
    }
  where
    p = getLocalWord 0
    tso_p = UnresolvedGetGlobal {unresolvedGlobalReg = CurrentTSO}
    task_p = getFieldWord mainCap offset_Capability_running_task
    guard_struct struct_addr_expr struct_size allowed_field_offsets =
      Binary
        { binaryOp = AndInt32
        , operand0 =
            Binary
              { binaryOp = AndInt32
              , operand0 =
                  Binary
                    { binaryOp = GeUInt64
                    , operand0 = p
                    , operand1 = struct_addr_expr
                    }
              , operand1 =
                  Binary
                    { binaryOp = LtUInt64
                    , operand0 = p
                    , operand1 = struct_field_off struct_size
                    }
              }
        , operand1 =
            notExpr $
            V.foldl' (Binary OrInt32) (ConstI32 0) $
            V.fromList
              [ Binary
                { binaryOp = EqInt64
                , operand0 =
                    Binary
                      { binaryOp = SubInt64
                      , operand0 = p
                      , operand1 = struct_addr_expr
                      }
                , operand1 = constInt o
                }
              | o <- allowed_field_offsets
              ]
        }
      where
        struct_field_off o =
          Binary
            { binaryOp = AddInt64
            , operand0 = struct_addr_expr
            , operand1 = constInt o
            }

loadWrapperFunction, storeWrapperFunction ::
     BuiltinsOptions -> BinaryenIndex -> ValueType -> AsteriusFunction
loadWrapperFunction _ b vt =
  AsteriusFunction
    { functionType = FunctionType {returnType = vt, paramTypes = [I64]}
    , body =
        Block
          { name = ""
          , bodys =
              [ CallImport
                  { target' =
                      case (vt, b) of
                        (I32, 1) -> "__asterius_load_i8"
                        (I32, 2) -> "__asterius_load_i16"
                        (I32, 4) -> "__asterius_load_i32"
                        (I64, 8) -> "__asterius_load_i64"
                        (F32, 4) -> "__asterius_load_f32"
                        (F64, 8) -> "__asterius_load_f64"
                        _ ->
                          error $
                          "Unsupported ValueType/ByteLength: " <> show (vt, b)
                  , operands =
                      cutI64 p <>
                      (case vt of
                         I64 -> cutI64 v
                         _ -> [v])
                  , valueType = None
                  }
              , Call
                  { target = "__asterius_memory_trap"
                  , operands = [p]
                  , valueType = None
                  }
              , v
              ]
          , valueType = vt
          }
    }
  where
    p = getLocalWord 0
    v =
      Load
        { signed = False
        , bytes = b
        , offset = 0
        , align = 0
        , valueType = vt
        , ptr = wrapI64 p
        }

storeWrapperFunction _ b vt =
  AsteriusFunction
    { functionType = FunctionType {returnType = None, paramTypes = [I64, vt]}
    , body =
        Block
          { name = ""
          , bodys =
              [ CallImport
                  { target' =
                      case (vt, b) of
                        (I32, 1) -> "__asterius_store_i8"
                        (I32, 2) -> "__asterius_store_i16"
                        (I32, 4) -> "__asterius_store_i32"
                        (I64, 8) -> "__asterius_store_i64"
                        (F32, 4) -> "__asterius_store_f32"
                        (F64, 8) -> "__asterius_store_f64"
                        _ ->
                          error $
                          "Unsupported ValueType/ByteLength: " <> show (vt, b)
                  , operands =
                      cutI64 p <>
                      (case vt of
                         I64 -> cutI64 v
                         _ -> [v])
                  , valueType = None
                  }
              , Call
                  { target = "__asterius_memory_trap"
                  , operands = [p]
                  , valueType = None
                  }
              , Store
                  { bytes = b
                  , offset = 0
                  , align = 0
                  , ptr = wrapI64 p
                  , value = v
                  , valueType = vt
                  }
              ]
          , valueType = None
          }
    }
  where
    p = getLocalWord 0
    v = GetLocal {index = 1, valueType = vt}

fieldOff :: Expression -> Int -> Expression
fieldOff p o
  | o == 0 = p
  | otherwise =
    Binary {binaryOp = AddInt64, operand0 = p, operand1 = constInt o}

getFieldWord :: Expression -> Int -> Expression
getFieldWord p o = loadWord (wrapI64 $ fieldOff p o)

loadWord :: Expression -> Expression
loadWord p =
  Load
    {signed = False, bytes = 8, offset = 0, align = 0, valueType = I64, ptr = p}

wrapI64 :: Expression -> Expression
wrapI64 w = Unary {unaryOp = WrapInt64, operand0 = w}

constInt :: Int -> Expression
constInt = ConstI64 . fromIntegral

getLocalWord :: BinaryenIndex -> Expression
getLocalWord i = GetLocal {index = i, valueType = I64}

mainCap :: Expression
mainCap = Unresolved {unresolvedSymbol = "MainCapability"}

offset_StgTSO_StgStack :: Int
offset_StgTSO_StgStack = 8 * roundup_bytes_to_words sizeof_StgTSO

cutI64 :: Expression -> V.Vector Expression
cutI64 x =
  [ wrapI64 x
  , wrapI64 $
    Binary {binaryOp = ShrUInt64, operand0 = x, operand1 = ConstI64 32}
  ]

notExpr :: Expression -> Expression
notExpr = Binary XorInt32 (ConstI32 0xFFFFFFFF)
