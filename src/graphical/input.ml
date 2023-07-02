module All_keyboard = struct
  type 'a t = {
    key_a : 'a;
    key_b : 'a;
    key_c : 'a;
    key_d : 'a;
    key_e : 'a;
    key_f : 'a;
    key_g : 'a;
    key_h : 'a;
    key_i : 'a;
    key_j : 'a;
    key_k : 'a;
    key_l : 'a;
    key_m : 'a;
    key_n : 'a;
    key_o : 'a;
    key_p : 'a;
    key_q : 'a;
    key_r : 'a;
    key_s : 'a;
    key_t : 'a;
    key_u : 'a;
    key_v : 'a;
    key_w : 'a;
    key_x : 'a;
    key_y : 'a;
    key_z : 'a;
    key_semicolon : 'a;
    key_apostrophe : 'a;
    key_comma : 'a;
    key_period : 'a;
    key_space : 'a;
    key_1 : 'a;
    key_2 : 'a;
    key_3 : 'a;
    key_4 : 'a;
    key_5 : 'a;
    key_6 : 'a;
    key_7 : 'a;
    key_8 : 'a;
    key_9 : 'a;
    key_0 : 'a;
  }

  let init ~f =
    {
      key_a = f ();
      key_b = f ();
      key_c = f ();
      key_d = f ();
      key_e = f ();
      key_f = f ();
      key_g = f ();
      key_h = f ();
      key_i = f ();
      key_j = f ();
      key_k = f ();
      key_l = f ();
      key_m = f ();
      key_n = f ();
      key_o = f ();
      key_p = f ();
      key_q = f ();
      key_r = f ();
      key_s = f ();
      key_t = f ();
      key_u = f ();
      key_v = f ();
      key_w = f ();
      key_x = f ();
      key_y = f ();
      key_z = f ();
      key_semicolon = f ();
      key_apostrophe = f ();
      key_comma = f ();
      key_period = f ();
      key_space = f ();
      key_1 = f ();
      key_2 = f ();
      key_3 = f ();
      key_4 = f ();
      key_5 = f ();
      key_6 = f ();
      key_7 = f ();
      key_8 = f ();
      key_9 = f ();
      key_0 = f ();
    }

  let map t ~f =
    {
      key_a = f t.key_a;
      key_b = f t.key_b;
      key_c = f t.key_c;
      key_d = f t.key_d;
      key_e = f t.key_e;
      key_f = f t.key_f;
      key_g = f t.key_g;
      key_h = f t.key_h;
      key_i = f t.key_i;
      key_j = f t.key_j;
      key_k = f t.key_k;
      key_l = f t.key_l;
      key_m = f t.key_m;
      key_n = f t.key_n;
      key_o = f t.key_o;
      key_p = f t.key_p;
      key_q = f t.key_q;
      key_r = f t.key_r;
      key_s = f t.key_s;
      key_t = f t.key_t;
      key_u = f t.key_u;
      key_v = f t.key_v;
      key_w = f t.key_w;
      key_x = f t.key_x;
      key_y = f t.key_y;
      key_z = f t.key_z;
      key_semicolon = f t.key_semicolon;
      key_apostrophe = f t.key_apostrophe;
      key_comma = f t.key_comma;
      key_period = f t.key_period;
      key_space = f t.key_space;
      key_1 = f t.key_1;
      key_2 = f t.key_2;
      key_3 = f t.key_3;
      key_4 = f t.key_4;
      key_5 = f t.key_5;
      key_6 = f t.key_6;
      key_7 = f t.key_7;
      key_8 = f t.key_8;
      key_9 = f t.key_9;
      key_0 = f t.key_0;
    }

  let get_by_sdl_scancode t scancode =
    let open Sdlscancode in
    match scancode with
    | A -> Some t.key_a
    | B -> Some t.key_b
    | C -> Some t.key_c
    | D -> Some t.key_d
    | E -> Some t.key_e
    | F -> Some t.key_f
    | G -> Some t.key_g
    | H -> Some t.key_h
    | I -> Some t.key_i
    | J -> Some t.key_j
    | K -> Some t.key_k
    | L -> Some t.key_l
    | M -> Some t.key_m
    | N -> Some t.key_n
    | O -> Some t.key_o
    | P -> Some t.key_p
    | Q -> Some t.key_q
    | R -> Some t.key_r
    | S -> Some t.key_s
    | T -> Some t.key_t
    | U -> Some t.key_u
    | V -> Some t.key_v
    | W -> Some t.key_w
    | X -> Some t.key_x
    | Y -> Some t.key_y
    | Z -> Some t.key_z
    | SEMICOLON -> Some t.key_semicolon
    | APOSTROPHE -> Some t.key_apostrophe
    | COMMA -> Some t.key_comma
    | PERIOD -> Some t.key_period
    | SPACE -> Some t.key_space
    | Num1 -> Some t.key_1
    | Num2 -> Some t.key_2
    | Num3 -> Some t.key_3
    | Num4 -> Some t.key_4
    | Num5 -> Some t.key_5
    | Num6 -> Some t.key_6
    | Num7 -> Some t.key_7
    | Num8 -> Some t.key_8
    | Num9 -> Some t.key_9
    | Num0 -> Some t.key_0
    | _ -> None
end

module Mouse_pos = struct
  type 'a t = { mouse_x : 'a; mouse_y : 'a }

  let init ~f = { mouse_x = f (); mouse_y = f () }
  let map t ~f = { mouse_x = f t.mouse_x; mouse_y = f t.mouse_y }
end

type ('keyboard, 'mouse) t = {
  keyboard : 'keyboard All_keyboard.t;
  mouse : 'mouse Mouse_pos.t;
}
