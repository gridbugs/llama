module Message = struct
  let is_status_byte int = int land 0x80 != 0
  let system_exclusive_start = 0b11110000
  let system_exclusive_end = 0b11110111

  type payload_length = Fixed of int | Variable

  let payload_length_of_status_byte int =
    assert (is_status_byte int);
    if int land 0x40 == 0 then
      let identifier = (int lsr 4) land 0x7 in
      let length =
        match identifier with
        | 0 -> 2
        | 1 -> 2
        | 2 -> 2
        | 3 -> 2
        | 4 -> 1
        | 5 -> 1
        | 6 -> 2
        | _ -> failwith "unreachable"
      in
      Fixed length
    else
      let identifier = int land 0xF in
      match identifier with
      | 0 -> Variable
      | 0b0010 -> Fixed 2
      | 0b0011 -> Fixed 1
      | _ -> Fixed 0
end
