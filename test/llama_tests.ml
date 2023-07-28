module Llama_midi = struct
  module Byte_array_parser = struct
    open Llama_midi.For_test.Byte_array_parser

    let%test _ = String.equal "abcd" @@ run string4 [| 'a'; 'b'; 'c'; 'd' |]
    let%test _ = Int.equal 259 @@ run int16be [| char_of_int 1; char_of_int 3 |]

    let%test_module _ =
      (module struct
        let make ints =
          run variable_length_quantity
            (Array.map char_of_int (Array.of_list ints))

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
  end
end
