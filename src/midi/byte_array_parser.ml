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
      loop (x :: acc) i
  in
  let xs, i = loop [] i in
  (List.rev xs, i)

let repeat_until_end_exact t a i =
  let rec loop acc i =
    if i >= Array.length a then (acc, i)
    else
      let x, i = t a i in
      loop (x :: acc) i
  in
  let xs, i = loop [] i in
  (List.rev xs, i)

let read_seq4_at a index = Seq.init 4 (fun i -> Array.get a (index + i))
let read_seq2_at a index = Seq.init 2 (fun i -> Array.get a (index + i))
let read_string4_at a index = read_seq4_at a index |> String.of_seq

let read_int32be_at a index =
  read_seq4_at a index
  |> Seq.fold_lefti (fun acc i ch -> acc + (int_of_char ch lsl ((3 - i) * 8))) 0

let read_int16be_at a index =
  read_seq2_at a index
  |> Seq.fold_lefti (fun acc i ch -> acc + (int_of_char ch lsl ((1 - i) * 8))) 0

let read_byte a index = int_of_char (Array.get a index)
let string4 a i = (read_string4_at a i, i + 4)
let int32be a i = (read_int32be_at a i, i + 4)
let int16be a i = (read_int16be_at a i, i + 2)
let byte a i = (read_byte a i, i + 1)
let peek_byte a i = (read_byte a i, i)

let byte_msb0 ~message =
  let+ byte = byte in
  if byte > 127 then
    raise
      (Parse_exception
         (Printf.sprintf "Expected byte with bit 7 set to 0 but got %d: (%s)"
            byte message));
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
      if this_byte land mask == 0 then (acc, i) else loop acc (count + 1) i
  in
  loop 0 0 i

let run t a = fst (t a 0)

let%test _ = String.equal "abcd" @@ run string4 [| 'a'; 'b'; 'c'; 'd' |]
let%test _ = Int.equal 259 @@ run int16be [| char_of_int 1; char_of_int 3 |]

let%test_module _ =
  (module struct
    let make ints =
      run variable_length_quantity (Array.map char_of_int (Array.of_list ints))

    let%test _ = Int.equal 0 @@ make [ 0 ]
    let%test _ = Int.equal 0 @@ make [ 0; 0; 0 ]
    let%test _ = Int.equal 0x40 @@ make [ 0x40 ]
    let%test _ = Int.equal 0x7F @@ make [ 0x7F ]
    let%test _ = Int.equal 0x80 @@ make [ 0x81; 0x00 ]
    let%test _ = Int.equal 0x2000 @@ make [ 0xC0; 0x00 ]
    let%test _ = Int.equal 0x3FFF @@ make [ 0xFF; 0x7F ]
    let%test _ = Int.equal 0x4000 @@ make [ 0x81; 0x80; 0x00 ]
    let%test _ = Int.equal 0x100000 @@ make [ 0xC0; 0x80; 0x00 ]
    let%test _ = Int.equal 0x1FFFFF @@ make [ 0xFF; 0xFF; 0x7F ]
    let%test _ = Int.equal 0x200000 @@ make [ 0x81; 0x80; 0x80; 0x00 ]
    let%test _ = Int.equal 0x8000000 @@ make [ 0xC0; 0x80; 0x80; 0x00 ]
    let%test _ = Int.equal 0xFFFFFFF @@ make [ 0xFF; 0xFF; 0xFF; 0x7F ]
  end)
