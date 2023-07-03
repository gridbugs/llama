module All_keyboard : sig
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

  val init : f:(unit -> 'a) -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Mouse_pos : sig
  type 'a t = { mouse_x : 'a; mouse_y : 'a }

  val init : f:(unit -> 'a) -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

type ('keyboard, 'mouse) t = {
  keyboard : 'keyboard All_keyboard.t;
  mouse : 'mouse Mouse_pos.t;
}
