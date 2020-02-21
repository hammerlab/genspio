include Base

module Unique_name = struct
  let x = ref 0

  let create prefix =
    Caml.incr x ;
    Fmt.str "%s_%d_%d" prefix !x (Random.int 100_000)

  let variable = create
end

let with_buffer ?(size = 42) f =
  let b = Buffer.create size in
  let str = Buffer.add_string b in
  let res = f str in
  (Buffer.contents b, res)
