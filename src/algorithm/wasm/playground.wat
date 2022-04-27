
(module

(memory $0 10000)
 (export "memory" (memory $0))
 (export "bounding_box_flat_f32" (func $bounding_box_flat_f32))
 (func $bounding_box_flat_f32 (; 0 ;) (param $0 i32) (param $1 i32)
  (local $2 i32)
  (local $3 f32)
  (local $4 f32)
  (local $5 f32)
  (local $6 f32)
  (local $7 f32)
  (block $label$0
   (block $label$1
    (br_if $label$1
     (i32.lt_s
      (local.tee $2
       (i32.shr_s
        (local.get $1)
        (i32.const 2)
       )
      )
      (i32.const 1)
     )
    )
    (local.set $6
     (f32.const -inf)
    )
    (local.set $1
     (i32.const 0)
    )
    (local.set $4
     (f32.const inf)
    )
    (local.set $5
     (f32.const inf)
    )
    (local.set $7
     (f32.const -inf)
    )
    (loop $label$2
     (local.set $6
      (select
       (local.get $6)
       (local.tee $3
        (f32.load
         (i32.add
          (local.get $0)
          (i32.const 4)
         )
        )
       )
       (f32.lt
        (local.get $3)
        (local.get $6)
       )
      )
     )
     (local.set $5
      (select
       (local.get $5)
       (local.get $3)
       (f32.gt
        (local.get $3)
        (local.get $5)
       )
      )
     )
     (local.set $7
      (select
       (local.get $7)
       (local.tee $3
        (f32.load
         (local.get $0)
        )
       )
       (f32.lt
        (local.get $3)
        (local.get $7)
       )
      )
     )
     (local.set $4
      (select
       (local.get $4)
       (local.get $3)
       (f32.gt
        (local.get $3)
        (local.get $4)
       )
      )
     )
     (local.set $0
      (i32.add
       (local.get $0)
       (i32.const 8)
      )
     )
     (br_if $label$2
      (i32.lt_s
       (local.tee $1
        (i32.add
         (local.get $1)
         (i32.const 2)
        )
       )
       (local.get $2)
      )
     )
     (br $label$0)
    )
   )
   (local.set $5
    (f32.const inf)
   )
   (local.set $7
    (f32.const -inf)
   )
   (local.set $6
    (f32.const -inf)
   )
   (local.set $4
    (f32.const inf)
   )
  )
  (f32.store
   (i32.const 0)
   (local.get $4)
  )
  (f32.store offset=4
   (i32.const 0)
   (local.get $5)
  )
  (f32.store offset=8
   (i32.const 0)
   (local.get $7)
  )
  (f32.store offset=12
   (i32.const 0)
   (local.get $6)
  )
  (f32.store offset=16
   (i32.const 0)
   (local.get $4)
  )
  (f32.store offset=20
   (i32.const 0)
   (local.get $5)
  )
  (f32.store offset=24
   (i32.const 0)
   (local.get $7)
  )
  (f32.store offset=28
   (i32.const 0)
   (local.get $6)
  )
 )
)
