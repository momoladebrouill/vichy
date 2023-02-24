let w = 800
let h = 800

let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  Random.self_init ()

let () =
  setup ();
  Ophelie.ophelie w h;
  (* Uncomment someone to play with, sale shrub*)
  Lilian.lilian w h;
  (*Ophelie.ophelie ();*)
  (*Vichy.vichy ();*)
