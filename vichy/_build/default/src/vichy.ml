let w = 500
let h = 500
let size = 20
type player = { pos : int*int}
type box = { pos : int*int}
type obj = Player of player | Box of box

type gamestate =
    { objects : obj list;
    prevkeys : (bool*bool*bool*bool)}
let setup () =
    Raylib.init_window w h "Vichy";
    Raylib.set_target_fps 60;
    {objects = [Player {pos = (w/2,h/2)};Box {pos=(w/2-size,h/2-size)}];prevkeys = (false,false,false,false)}


let rec loop gamestate =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
        let open Raylib in
        begin_drawing ();
        clear_background Color.black;

        List.iter 
        (fun obj -> 

            match obj with
            Player p ->  let x,y = p.pos in draw_rectangle x y size size Color.raywhite
            | Box b -> let x,y = b.pos  in  draw_rectangle x y size size Color.red   

        ) gamestate.objects;

        end_drawing ();
    let w,a,s,d = gamestate.prevkeys in
    let nw, na, ns, nd = (is_key_down Key.W,is_key_down Key.A,is_key_down Key.S,is_key_down Key.D) in
    loop {
        objects = 
            (List.map (fun obj ->
                match obj with
                |Player p -> let x,y = p.pos in 
                if nw && not w then Player {pos=(x,y-size)} 
                else if na && not a then Player {pos=(x-size,y)} 
                else if ns && not s then Player {pos=(x,y+size)} 
                else if nd && not d then Player {pos=(x+size,y)} 
                else Player p
                | Box b -> Box b) 
            gamestate.objects);
        prevkeys = (nw,na,ns,nd) 
                }
let () = loop (setup ())
