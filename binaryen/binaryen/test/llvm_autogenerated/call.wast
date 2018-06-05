(module
 (type $FUNCSIG$v (func))
 (type $FUNCSIG$i (func (result i32)))
 (type $FUNCSIG$vi (func (param i32)))
 (type $FUNCSIG$iii (func (param i32 i32) (result i32)))
 (type $FUNCSIG$ii (func (param i32) (result i32)))
 (type $FUNCSIG$j (func (result i64)))
 (type $FUNCSIG$f (func (result f32)))
 (type $FUNCSIG$d (func (result f64)))
 (import "env" "double_nullary" (func $double_nullary (result f64)))
 (import "env" "float_nullary" (func $float_nullary (result f32)))
 (import "env" "i32_binary" (func $i32_binary (param i32 i32) (result i32)))
 (import "env" "i32_nullary" (func $i32_nullary (result i32)))
 (import "env" "i32_unary" (func $i32_unary (param i32) (result i32)))
 (import "env" "i64_nullary" (func $i64_nullary (result i64)))
 (import "env" "void_nullary" (func $void_nullary))
 (import "env" "memory" (memory $0 1))
 (table 0 anyfunc)
 (data (i32.const 4) "\10\04\00\00")
 (export "call_i32_nullary" (func $call_i32_nullary))
 (export "call_i64_nullary" (func $call_i64_nullary))
 (export "call_float_nullary" (func $call_float_nullary))
 (export "call_double_nullary" (func $call_double_nullary))
 (export "call_void_nullary" (func $call_void_nullary))
 (export "call_i32_unary" (func $call_i32_unary))
 (export "call_i32_binary" (func $call_i32_binary))
 (export "call_indirect_void" (func $call_indirect_void))
 (export "call_indirect_i32" (func $call_indirect_i32))
 (export "call_indirect_arg" (func $call_indirect_arg))
 (export "call_indirect_arg_2" (func $call_indirect_arg_2))
 (export "tail_call_void_nullary" (func $tail_call_void_nullary))
 (export "fastcc_tail_call_void_nullary" (func $fastcc_tail_call_void_nullary))
 (export "coldcc_tail_call_void_nullary" (func $coldcc_tail_call_void_nullary))
 (export "stackSave" (func $stackSave))
 (export "stackAlloc" (func $stackAlloc))
 (export "stackRestore" (func $stackRestore))
 (func $call_i32_nullary (; 7 ;) (result i32)
  (return
   (call $i32_nullary)
  )
 )
 (func $call_i64_nullary (; 8 ;) (result i64)
  (return
   (call $i64_nullary)
  )
 )
 (func $call_float_nullary (; 9 ;) (result f32)
  (return
   (call $float_nullary)
  )
 )
 (func $call_double_nullary (; 10 ;) (result f64)
  (return
   (call $double_nullary)
  )
 )
 (func $call_void_nullary (; 11 ;)
  (call $void_nullary)
  (return)
 )
 (func $call_i32_unary (; 12 ;) (param $0 i32) (result i32)
  (return
   (call $i32_unary
    (get_local $0)
   )
  )
 )
 (func $call_i32_binary (; 13 ;) (param $0 i32) (param $1 i32) (result i32)
  (return
   (call $i32_binary
    (get_local $0)
    (get_local $1)
   )
  )
 )
 (func $call_indirect_void (; 14 ;) (param $0 i32)
  (call_indirect (type $FUNCSIG$v)
   (get_local $0)
  )
  (return)
 )
 (func $call_indirect_i32 (; 15 ;) (param $0 i32) (result i32)
  (return
   (call_indirect (type $FUNCSIG$i)
    (get_local $0)
   )
  )
 )
 (func $call_indirect_arg (; 16 ;) (param $0 i32) (param $1 i32)
  (call_indirect (type $FUNCSIG$vi)
   (get_local $1)
   (get_local $0)
  )
  (return)
 )
 (func $call_indirect_arg_2 (; 17 ;) (param $0 i32) (param $1 i32) (param $2 i32)
  (drop
   (call_indirect (type $FUNCSIG$iii)
    (get_local $1)
    (get_local $2)
    (get_local $0)
   )
  )
  (return)
 )
 (func $tail_call_void_nullary (; 18 ;)
  (call $void_nullary)
  (return)
 )
 (func $fastcc_tail_call_void_nullary (; 19 ;)
  (call $void_nullary)
  (return)
 )
 (func $coldcc_tail_call_void_nullary (; 20 ;)
  (call $void_nullary)
  (return)
 )
 (func $stackSave (; 21 ;) (result i32)
  (i32.load offset=4
   (i32.const 0)
  )
 )
 (func $stackAlloc (; 22 ;) (param $0 i32) (result i32)
  (local $1 i32)
  (i32.store offset=4
   (i32.const 0)
   (tee_local $1
    (i32.and
     (i32.sub
      (i32.load offset=4
       (i32.const 0)
      )
      (get_local $0)
     )
     (i32.const -16)
    )
   )
  )
  (get_local $1)
 )
 (func $stackRestore (; 23 ;) (param $0 i32)
  (i32.store offset=4
   (i32.const 0)
   (get_local $0)
  )
 )
)
;; METADATA: { "asmConsts": {},"staticBump": 1040, "initializers": [], "declares": ["double_nullary","float_nullary","i32_binary","i32_nullary","i32_unary","i64_nullary","void_nullary"], "externs": [], "implementedFunctions": ["_call_i32_nullary","_call_i64_nullary","_call_float_nullary","_call_double_nullary","_call_void_nullary","_call_i32_unary","_call_i32_binary","_call_indirect_void","_call_indirect_i32","_call_indirect_arg","_call_indirect_arg_2","_tail_call_void_nullary","_fastcc_tail_call_void_nullary","_coldcc_tail_call_void_nullary","_stackSave","_stackAlloc","_stackRestore"], "exports": ["call_i32_nullary","call_i64_nullary","call_float_nullary","call_double_nullary","call_void_nullary","call_i32_unary","call_i32_binary","call_indirect_void","call_indirect_i32","call_indirect_arg","call_indirect_arg_2","tail_call_void_nullary","fastcc_tail_call_void_nullary","coldcc_tail_call_void_nullary","stackSave","stackAlloc","stackRestore"], "invokeFuncs": [] }