exception Parse_exception of string

type 'a t = char array -> int -> 'a * int

let map t ~f a i =
  let x, i = t a i in
  (f x, i)

let both x y a i =
  let x_, i = x a i in
  let y_, i = y a i in
  ((x_, y_), i)

let bind t ~f a i =
  let x, i = t a i in
  f x a i

let ( >>| ) t f = map t ~f
let ( >>= ) t f = bind t ~f
let ( let+ ) = ( >>| )
let ( and+ ) = both
let ( let* ) = ( >>= )
let return x _ i = (x, i)
let skip n _ i = ((), i + n)

let repeat_until_relative_index_offset_exact offset t a i =
  let absolute_index_threshold = i + offset in
  let rec loop acc i =
    if i == absolute_index_threshold then (acc, i)
    else if i > absolute_index_threshold then
      raise (Parse_exception "Buffer region not aligned to contents")
    else
      let x, i = t a i in
      (loop [@tailcall]) (x :: acc) i
  in
  let xs, i = loop [] i in
  (List.rev xs, i)

let repeat_until_end_exact t a i =
  let rec loop acc i =
    if i >= Array.length a then (acc, i)
    else
      let x, i = t a i in
      (loop [@tailcall]) (x :: acc) i
  in
  let xs, i = loop [] i in
  (List.rev xs, i)

let read_string n a index =
  let[@tail_mod_cons] rec loop i =
    if i == n then [] else Array.get a (index + i) :: loop (i + 1)
  in
  loop 0 |> List.to_seq |> String.of_seq

let read_string4 = read_string 4

let read_int_be n a index =
  let rec loop acc i =
    if i == n then acc
    else
      let byte = int_of_char (Array.get a (index + i)) in
      let value = (acc lsl 8) lor byte in
      loop value (i + 1)
  in
  loop 0 0

let read_int32be = read_int_be 4
let read_int16be = read_int_be 2
let read_byte a index = int_of_char (Array.get a index)
let string4 a i = (read_string4 a i, i + 4)
let int32be a i = (read_int32be a i, i + 4)
let int16be a i = (read_int16be a i, i + 2)
let byte a i = (read_byte a i, i + 1)
let peek_byte a i = (read_byte a i, i)

let byte_msb0 =
  let+ byte = byte in
  if byte > 127 then
    raise
      (Parse_exception
         (Printf.sprintf "Expected byte with bit 7 set to 0 but got %d" byte));
  byte

let n_bytes n a i =
  let out = Array.init n (Fun.const (char_of_int 0)) in
  Array.blit a i out 0 n;
  (out, i + n)

let variable_length_quantity a i =
  let rec loop acc count i =
    if count >= 4 then
      raise
        (Parse_exception
           "read more than 4 bytes while parsing variable length quantity")
    else
      let this_byte, i = byte a i in
      let mask = 0x80 in
      let this_byte_value = this_byte land lnot mask in
      let acc = (acc lsl 7) lor this_byte_value in
      if this_byte land mask == 0 then (acc, i)
      else (loop [@tailcall]) acc (count + 1) i
  in
  loop 0 0 i

let run t a = fst (t a 0)
