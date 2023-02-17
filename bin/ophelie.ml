let w = 500
let h = 500
let size = 50
let qqty = 2
type player  = {pos:int*int}
type box = {pos:int*int;ind : int}
type obj = Player of player | Box of box

type gamestate = {
    objects : obj list;
    prevkeys : (bool*bool*bool*bool);
    player : player;
}

let setup () =
    Raylib.init_window w h "Vichy";
    Raylib.set_target_fps 60;
    {
        player =  {pos=(w/2,h/2)};
        objects = [
            Box {pos=(w/2-size,h/2-size);ind=1};
            Box {pos=(w/2+size,h/2+size);ind=2}];
        prevkeys = (false,false,false,false);
        }
let get_pos obj =
    match obj with
    | Player p -> p.pos
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

let force_move obj dir =
    match obj with
    | Player p -> Player {pos = p.pos $+ dir}
    | Box b -> Box {pos = b.pos $+ dir;ind=b.ind}

let rec move obj dir objs =
    match find_at ((get_pos obj) $+ dir) objs with
    | None -> [force_move obj dir]
    | Some o -> (force_move obj dir)::(move o dir objs)
   
let rec find_with_ind i objs =
    match objs with
    [] -> None
    | t::q -> match t with 
    |Player _ -> find_with_ind i q
    |Box t -> if t.ind = i then Some (Box t) else find_with_ind i q

let rec concat nobjs objs i =
    if i=0 then [] else
    (match find_with_ind i nobjs with
    None -> (match find_with_ind i objs with None -> failwith "lost forever"  | Some o -> o)
    | Some o -> o)::(concat nobjs objs (i-1))

let rec loop gamestate =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
        let open Raylib in
        begin_drawing ();
        clear_background Color.black;

        List.iter (fun obj -> 
            match obj with
            | Player p -> let x,y = p.pos in draw_rectangle x y size size Color.white
            | Box b -> let x,y = b.pos  in  draw_rectangle x y size size Color.red   
        ) ((Player gamestate.player)::gamestate.objects);

        end_drawing ();

    let w,a,s,d = gamestate.prevkeys in
    let nw, na, ns, nd = (is_key_down Key.W,is_key_down Key.A,is_key_down Key.S,is_key_down Key.D) in
    let objects = gamestate.objects in
    let nobjects = (
                if nw && not w then (move (Player gamestate.player) (0,-size) objects)
                else if na && not a then (move (Player gamestate.player) (-size,0) objects)
                else if ns && not s then (move (Player gamestate.player) (0,size) objects)
                else if nd && not d then (move (Player gamestate.player) (size,0) objects)
                else ((Player gamestate.player)::gamestate.objects))
    in
    let player = match List.hd nobjects with Player p -> {pos = p.pos} | _ -> gamestate.player in
    
    let gamestate' = {
        objects = concat (List.tl nobjects) objects qqty;
        prevkeys = (nw,na,ns,nd);
        player = player; 
    }
    in loop gamestate'
let ophelie () = loop (setup ())
