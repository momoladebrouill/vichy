open Raylib

let w = 700
let h = 700
let text_width = 14
let text_height = 30
let abs_path = "./src/textes/" 


let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;

  let font = load_font_ex (abs_path^"font.ttf") 600 None in
  gen_texture_mipmaps (addr (Font.texture font));
  set_texture_filter (Font.texture font) TextureFilter.Trilinear;
  set_target_fps 60;
  Random.self_init ();
  font

let get_char cl = really_input_string cl 1
(*
    let c = input_char cl in
    let v = Char.code c in
    let cs = String.make 1 c in
    if 60<=v && v<=122 || c = ' ' || c = '\n' ||c='?' then cs
    else cs^(get_char cl)*)

type state = Waiting | Writting 

type text_env = { 
    state : state;
    c : string;
    x : int;
    y : int;
}

let rec text t font=
  let rec text_aux cs time =
    if Raylib.window_should_close () then Raylib.close_window ();
    Raylib.begin_drawing ();
    let v =  (Raylib.Vector2.create (float_of_int cs.x) (float_of_int cs.y)) in
    (*draw_text_codepoint font (Char.code cs.c) v 30. Color.raywhite;*)
    Raylib.draw_text_ex 
      font  cs.c v 30. 0.0 Raylib.Color.raywhite;
    Raylib.end_drawing ();
    let state = if Raylib.is_key_pressed Raylib.Key.Space 
      then  Writting else cs.state in
    match state with 
    | Writting ->
            begin
            try (
               (* Juste avant on attendais, maintenant on écrit => c'est une nouvelle partie,sur le même in_channel*)
              if cs.state = Waiting then text t font else
              let triger_newline = cs.x + text_width > w || cs.c = "\n" in
              (if time mod 2 = 0 then let cs' =
                { 
                  state = if cs.c = "]" then Waiting else state;
                  c = get_char t ;
                  x = if triger_newline then text_width else cs.x+text_width;
                  y = if triger_newline then cs.y + text_height else cs.y;
                } in text_aux cs' 
            else text_aux cs) ((time +1) mod 60)) with End_of_file -> () | _ -> ();
            end
   | Waiting -> text_aux cs ((time+1) mod 60)
  in
  Raylib.clear_background Raylib.Color.black;
  text_aux {c=get_char t;x=text_width;y=text_height;state=Writting} 0

let () =
  let font =  setup () in 
  begin try 
  Marc.marc w h;
  text (open_in (abs_path ^"init.txt")) font;
  Lilian.lilian w h;
  text (open_in (abs_path ^"lilian_to_ophelie.txt")) font; 
  Ophelie.ophelie w h;
  text (open_in (abs_path ^"ophelie_to_marc.txt")) font; 
  Marc.marc w h;
  text (open_in (abs_path ^"end.txt")) font; 
  with Ophelie.Perdu | Lilian.Perdu | Marc.Perdu -> text (open_in (abs_path ^"perdu.txt")) font; | e -> raise e end;
  unload_font font;
