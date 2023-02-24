let w = 700
let h = 700
let text_width = 20
let text_height = 30

let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  Random.self_init ()

type state = Waiting | Writting | Escape
type text_env = { 
    state : state;
    c : char;
    x : int;
    y : int;
}

let rec text t =
  let rec text_aux cs time =
    if Raylib.window_should_close () then Raylib.close_window ();
    Raylib.begin_drawing ();
    Raylib.draw_text (String.make 1 cs.c) (cs.x) (cs.y) 30 Raylib.Color.raywhite;
    Raylib.end_drawing ();
    let state = if Raylib.is_key_pressed Raylib.Key.Space then (if Char.code cs.c = 0 then Escape else  Writting )else cs.state in
    match state with 
    | Escape -> ()
    | Writting ->
            begin
            try (
          if cs.state = Waiting then text t else
          let triger_newline = cs.x + text_width > w || cs.c = '\n' in
            (if time mod 2 = 0 then let cs' =
              { 
                state = if cs.c = ']' then Waiting else state;
                c = input_char t ;
                x = if triger_newline then text_width else cs.x+text_width;
                y = if triger_newline then cs.y + text_height else cs.y;
              } in text_aux cs' 
            else text_aux cs) ((time +1) mod 60)) with End_of_file -> () | _ -> ();
            end
   | Waiting -> text_aux cs ((time+1) mod 60)
  in
  Raylib.clear_background Raylib.Color.black;
  text_aux {c=input_char t;x=text_width;y=text_height;state=Writting} 0

let () =
  let abs_path = "../../../src" in
  setup ();
  text (open_in (abs_path ^"/textes/init.txt"));
  text (open_in (abs_path ^"/textes/end.txt")); 
  Lilian.lilian w h;
  Ophelie.ophelie w h;
  Marc.marc w h;
