(module 
  (import "system" "mem" (memory 100))
  (func $logI32 (import "system" "logI32") (param i32 i32))
  (func $logF64 (import "system" "logF64") (param f64))
  (global (mut i32) i32.const 0) 
  (export "main_0" (func $main_0))
  (func $main_0 (local $e i32)(local $f1 i32)(local $a i32)(local $b i32)(local $y1 i32)(local $x1 i32)(local $c i32)(local $d1 i32)
    global.get 0
    local.set $a
    global.get 0
    i32.const 0
    i32.store
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.get $a
    i32.const 4
    i32.add
    i32.const 20
    i32.store
    local.get $a
    local.set $x1
    global.get 0
    local.set $b
    global.get 0
    i32.const 1
    i32.store
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.get $b
    i32.const 4
    i32.add
    global.get 0
    i32.const 0
    i32.add
    i32.const 77
    i32.store8
    global.get 0
    i32.const 1
    i32.add
    i32.const 76
    i32.store8
    global.get 0
    i32.const 2
    i32.add
    i32.const 115
    i32.store8
    global.get 0
    i32.const 3
    i32.add
    i32.const 99
    i32.store8
    global.get 0
    i32.const 4
    i32.add
    i32.const 114
    i32.store8
    global.get 0
    i32.const 5
    i32.add
    i32.const 105
    i32.store8
    global.get 0
    i32.const 6
    i32.add
    i32.const 112
    i32.store8
    global.get 0
    i32.const 7
    i32.add
    i32.const 116
    i32.store8
    global.get 0
    i32.const 8
    i32.add
    i32.const 0
    i32.store8
    global.get 0
    i32.const 9
    i32.add
    i32.const 0
    i32.store8
    global.get 0
    i32.const 10
    i32.add
    i32.const 0
    i32.store8
    global.get 0
    i32.const 11
    i32.add
    i32.const 0
    i32.store8
    global.get 0
    global.get 0
    i32.const 12
    i32.add
    global.set 0
    i32.store
    local.get $b
    local.set $y1
    local.get $x1
    i32.const 4
    i32.add
    i32.load
    local.set $c
    local.get $c
    i32.const 2
    call $logI32
    i32.const 0
    local.set $d1
    local.get $y1
    i32.const 4
    i32.add
    i32.load
    local.set $e
    local.get $e
    i32.const 4
    call $logI32
    i32.const 0
    local.set $f1
  )
  (export "main" (func $main))
  (func $main 
    call $main_0
  )
)