let w = 500
let h = 500
let size = 50
type player = { pos : int*int}
type box = { pos : int*int}
type obj =  Player of player | Box of box | Wall of box

type gamestate = {
    objects : obj list;
    prevkeys : (bool*bool*bool*bool)
}

let setup () =
    Raylib.init_window w h "Vichy";
    Raylib.set_target_fps 60;
    {
        objects = [Player {pos = (w/2,h/2)};Box {pos=(w/2-size,h/2-size)}; Wall {pos=(w/2-3*size,h/2-3*size)}];
        prevkeys = (false,false,false,false);
        }
let get_pos obj =
    match obj with
    | Player p -> p.pos
    | Wall o -> o.pos
    | Box o -> o.pos

let ($+) a b =
    let xa,ya = a in
    let xb,yb = b in
    (xa+xb,ya+yb)

let ($=) a b =
    let xa,ya = a in
    let xb,yb = b in
    xa=xb && ya=yb

let  rec find_at pos objs=
    match objs with
    | [] -> None
    | t::q -> if (get_pos t) $= pos then Some t else find_at pos q

let  rec can_i_move obj dir objs =
    match (find_at ((get_pos obj) $+ dir) objs) with
    | None -> true
    | Some x -> match x with 
      | Wall _ -> false
      | x -> can_i_move x dir objs

(*let move obj dir objs =
    if can_i_move obj dir objs then get_pos obj else (get_pos obj) $+ dir
  *)  

let rec loop gamestate =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
        let open Raylib in
        begin_drawing ();
        clear_background Color.black;

        List.iter (fun obj -> 
            match obj with
            Player p ->  let x,y = p.pos in draw_rectangle x y size size Color.raywhite
            | Box b -> let x,y = b.pos  in  draw_rectangle x y size size Color.red   
            | Wall b -> let x,y = b.pos  in  draw_rectangle x y size size Color.gray  
        ) gamestate.objects;

        end_drawing ();
    let couilles = gamestate.objects in
    let w,a,s,d = gamestate.prevkeys in
    let nw, na, ns, nd = (is_key_down Key.W,is_key_down Key.A,is_key_down Key.S,is_key_down Key.D) in
    loop {
        objects = (List.map (fun obj ->
            match obj with
                |Player p -> let x,y = p.pos in 
                if nw && not w && can_i_move obj (0,-size) couilles then Player {pos=(x,y-size)} 
                else if na && not a && can_i_move obj (-size,0) couilles then Player {pos=(x-size,y)} 
                else if ns && not s && can_i_move obj (0,size) couilles then Player {pos=(x,y+size)} 
                else if nd && not d && can_i_move obj (size,0) couilles then Player {pos=(x+size,y)} 
                else Player p
                | Box b -> Box b
                | Wall w -> Wall w) 
        gamestate.objects);
        prevkeys = (nw,na,ns,nd) 
                }
let () = loop (setup ())
