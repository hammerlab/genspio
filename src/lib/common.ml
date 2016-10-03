
include Nonstd
module String = Sosa.Native_string

module Unique_name = struct

  let x = ref 0
  let create prefix =
    incr x;
    let now = Unix.gettimeofday () in
    sprintf "%s_%s_%d"
      prefix
      (truncate (1000. *. now) |> Int.to_string)
      !x

end

let with_buffer ?(size = 42) f =
  let b = Buffer.create 42 in
  let str = Buffer.add_string b in
  let res = f str in
  Buffer.contents b, res


