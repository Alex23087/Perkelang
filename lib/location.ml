type location = {
  start_pos : int * int;
  end_pos : int * int;
}
[@@deriving show]

let dummy_pos = { start_pos = (0, -1); end_pos = (0, -1) }

let to_code_position (start_position, end_position) =
  let start_pos =
    ( start_position.Lexing.pos_lnum,
      start_position.Lexing.pos_cnum - start_position.Lexing.pos_bol + 1 )
  in
  let end_pos =
    ( end_position.Lexing.pos_lnum,
      end_position.Lexing.pos_cnum - end_position.Lexing.pos_bol )
  in
  { start_pos; end_pos }

let equal_location loc1 loc2 =
  loc1.start_pos = loc2.start_pos && loc1.end_pos = loc2.end_pos
