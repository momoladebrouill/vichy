let w = 800
let h = 800

let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  Random.self_init ()

let text t =
  let rec text_aux t p =
    if Raylib.window_should_close () then Raylib.close_window ();
    Raylib.begin_drawing ();
    Raylib.clear_background Raylib.Color.black;
    Raylib.draw_text t (0) (0) 30 Raylib.Color.raywhite;
    Raylib.end_drawing ();
    if Raylib.is_key_down Raylib.Key.Space && not p then ()
    else text_aux t (Raylib.is_key_down Raylib.Key.Space)
  in text_aux t (Raylib.is_key_down Raylib.Key.Space)

let () =
  setup ();
  text "Bienvenue jeune fougueux !
  Alors, comme ça, vous voulez rejoindre l'X ?
  Pour cela, il vous faudra passer 
  des tests tumultueux,
  Qui ne vous laisseront pas sans cicatrice !
  [SPACE]";
  text "cougilis"; 
  Lilian.lilian w h;
  Ophelie.ophelie w h;
  Marc.marc w h;
