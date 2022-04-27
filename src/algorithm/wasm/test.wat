(module
  (import "console" "log" (func $log (param i32)))
  (memory 10000)

  ;; 0, 1, 2, 3 are the ultimate locations of minX, minY, maxX, maxY
  ;; data is read from the given start address, with a given length
  (func $bounding_box_flat_f32 (param $start_address i32) (param $data_length i32) (result i32)

    (local $end_address i32)
    (local $minimum_known v128)
    (local $maximum_known v128)
    (local $cmp v128)
    (local.set $end_address
        (i32.add (local.get $start_address) (local.get $data_length)))

    ;; Increment start_address by 4 each time until end_address
    (block $out
        (loop $main
            local.get $start_address

            v128.load                ;; get 4 items at a time
            local.set $cmp

            local.get $minimum_known
            local.get $cmp
            f32x4.pmin
            local.set $minimum_known

            local.get $maximum_known
            local.get $cmp
            f32x4.pmax
            local.set $maximum_known

            local.get $start_address
            i32.const 32
            i32.add
            local.set $start_address ;; 32 bytes/iter

            local.get $start_address
            local.get $end_address
            i32.ge_s

            (if (then br $out) (else br $main))
        )
     )

     i32.const 0
     
   )
  (export "bounding_box_flat_f32" (func $bounding_box_flat_f32))
)
