let w = 500
let h = 500
let size = 50
type player = { pos : int*int}
type box = { pos : int*int; speed:int}
type obj =  Player of player |  Wall of box 

type gamestate = {
    objects : obj list;
    prevkeys : (bool*bool*bool*bool);
    time : int;
}
(*
let gen_walls x' y' =
    let rec aux x y acc =
        (if x=y && x=0 then (fun a->a)
        else if x=0 then aux x' (y-1)
        else aux (x-1) y) (if Random.int 2 = 1 then acc else (Wall {pos=(1+x,y)})::acc) in
    aux x' y' []
*)
let rec gen_walls x acc =
    if x = 0 then acc else
        gen_walls (x-1) (if x mod 2 = 0 then acc else (Wall {pos=(x,0);speed=Random.int 4})::acc)
let setup () =
    Raylib.init_window w h "Vichy";
    Raylib.set_target_fps 60;
    {
        objects = gen_walls (w/size-2) [Player {pos = (0,0)}];
        prevkeys = (false,false,false,false);
        time = 0;
        }

let get_pos obj =
    match obj with
    | Player p -> p.pos
    | Wall o -> o.pos

let ($+) a b =
    let xa,ya = a in
    let xb,yb = b in
    (xa+xb,ya+yb)

let ($=) a b =
    let xa,ya = a in
    let xb,yb = b in
    xa=xb && ya=yb
let ($*) pos q = let x,y = pos in (x*q,y*q)

let  rec find_at pos objs=
    match objs with
    | [] -> None
    | t::q -> if (get_pos t) $= pos then Some t else find_at pos q

let can_i_move obj dir objs =
    match (find_at ((get_pos obj) $+ dir) objs) with
    | None -> true
    | Some _ -> false


let rec loop gamestate =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
        let open Raylib in
        begin_drawing ();
        clear_background Color.black;

        List.iter (fun obj -> 
            match obj with
            Player p ->  let x,y = p.pos $* size in draw_rectangle x y size size Color.raywhite
            | Wall b -> let x,y = b.pos $* size in  draw_rectangle x y size size Color.gray  
        ) gamestate.objects;

        end_drawing ();
    let couilles = gamestate.objects in
    let w,a,s,d = gamestate.prevkeys in
    let nw, na, ns, nd = (is_key_down Key.W,is_key_down Key.A,is_key_down Key.S,is_key_down Key.D) in
    loop {
        objects = List.map (fun obj ->
            match obj with
                |Player p -> let x,y = p.pos in 
                if nw && not w && can_i_move obj (0,-1) couilles then Player {pos=(x,y-1)} 
                else if na && not a && can_i_move obj (-1,0) couilles then Player {pos=(x-1,y)} 
                else if ns && not s && can_i_move obj (0,1) couilles then Player {pos=(x,y+1)} 
                else if nd && not d && can_i_move obj (1,0) couilles then Player {pos=(x+1,y)} 
                else Player p
                | Wall w -> if gamestate.time = w.speed then Wall {pos = (let x,y = w.pos in (x,(y+1) mod (h/size))) ; speed=w.speed} else Wall w) 
        gamestate.objects;
        prevkeys = (nw,na,ns,nd);
        time = (gamestate.time + 1) mod 4
                }
let lilian () = loop (setup ())
