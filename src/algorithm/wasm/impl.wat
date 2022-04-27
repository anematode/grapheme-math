(module
  (import "console" "log" (func $logi32 (param i32)))
  (import "console" "log" (func $logf32 (param f32)))
  (import "console" "log" (func $logf64 (param f64)))

  (memory $0 10000)

  ;; data is read from the given start address, with a given length
  (func $bounding_box_flat_f32 (param $start_address i32) (param $data_length i32)

    (local $end_address i32)
    (local $minimum_known v128)
    (local $maximum_known v128)
    (local $cmp v128)
    (local.set $minimum_known
        (v128.const f32x4 +inf +inf +inf +inf))
    (local.set $maximum_known
        (v128.const f32x4 -inf -inf -inf -inf))
    (local.set $end_address
        (i32.add (local.get $start_address) (local.get $data_length)))

    ;; Unrolled, 4 vec2 entries / iter
    (block $loop
        (loop $main
            ;; First 2 entries
            local.get $minimum_known
            local.get $start_address

            v128.load
            local.tee $cmp

            f32x4.pmin
            local.set $minimum_known

            local.get $maximum_known
            local.get $cmp
            f32x4.pmax
            local.set $maximum_known

            ;; Second 2 entries
            local.get $minimum_known

            ;; Incr
            local.get $start_address
            i32.const 16
            i32.add

            v128.load
            local.tee $cmp

            f32x4.pmin
            local.set $minimum_known

            local.get $maximum_known
            local.get $cmp
            f32x4.pmax
            local.set $maximum_known

            local.get $start_address
            i32.const 32
            i32.add

            local.tee $start_address

            local.get $end_address
            i32.ge_s

            (if (then br $loop) (else br $main))
        )
    )

    i32.const 0
    local.get $minimum_known
    v128.store
    i32.const 16
    local.get $maximum_known
    v128.store
  )
  (export "bounding_box_flat_f32" (func $bounding_box_flat_f32))
  (export "memory" (memory $0))
)
