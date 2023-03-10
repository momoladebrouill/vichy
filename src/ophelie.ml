(* Ophélie : pousser des blocs pour former une liste triée, les blocs sont blancs quand ils sont bien voisins avec leur n-1*)

let size = 50
let qqty = 10 (*nombre de blocs*)

type player  = {
    pos: int*int
}

type box = {
    pos: int*int; 
    ind: int
}

type obj = 
    Player of player
    | Box of box

exception Perdu

type gamestate = {
  objects : obj list;
  prevkeys : (bool*bool*bool*bool);
  player : player;
  time : int;
  camera : (int*int);
}

let rec generate n =
  if n = 0 then []
  else (Box {
      pos = ((Random.int 10) - 5, (Random.int 10) - 5);
      ind = n
  })::(generate (n-1))

let setup () =
  {
    player =  {pos = (0, 0)};
    objects = generate qqty;
    prevkeys = (false, false, false, false);
    camera = (0, 0);
    time = 60*60;
  }

let get_pos obj =
  match obj with
  | Player p -> p.pos
  | Box o -> o.pos

let ($+) a b =
  let xa,ya = a in
  let xb,yb = b in
  (xa + xb, ya + yb)

let ($=) a b =
  let xa, ya = a in
  let xb, yb = b in
  xa = xb && ya = yb

let ($*) pos q = let x, y = pos in (x * q, y * q)

(* calculer la position d'affichage en fonction de combien la caméra s'est déplacée*)
let shifted_of_absolute pos cam w h = (pos $* size) $+ (w / 2, h / 2) $+ (cam $* -1) 

let rec find_at pos objs =
  match objs with
  | [] -> None
  | t::q -> if (get_pos t) $= pos then Some t else find_at pos q

let force_move obj dir =
  match obj with
  | Player p -> Player {pos = p.pos $+ dir}
  | Box b -> Box {pos = b.pos $+ dir; ind = b.ind}

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

(* concaténation des nouveaux objets avec ceux qui ont été déplacés*)
let rec concat nobjs objs i =
  if i=0 then [] else
    (match find_with_ind i nobjs with
       None -> (match find_with_ind i objs with None -> failwith "lost forever" | Some o -> o)
     | Some o -> o)::(concat nobjs objs (i-1))

let is_neight_preced b objects =
  if b.ind = 1 then true else
  match find_at (b.pos $+ (-1,0)) objects with 
  Some (Box b') ->  let (x',y'),(x,y) = b'.pos,b.pos in  x' < x && y' = y && b'.ind = b.ind -1 
     | _ -> false

let rec sorted objects =
   match objects with
   | [] -> true
   | (Player _) ::q -> sorted q 
   | (Box b)::q -> if is_neight_preced b objects then sorted q else false 
   

let rec loop gamestate w h =
  if Raylib.window_should_close () then Raylib.close_window ()
  else if (sorted gamestate.objects) then ()
  else
    let open Raylib in
    
    begin_drawing ();
    clear_background Color.black;
    
    List.iter (fun obj -> 
        match obj with
        | Player p -> let x,y = shifted_of_absolute p.pos gamestate.camera w h in draw_rectangle x y size size Color.raywhite
        | Box b -> let x,y = shifted_of_absolute b.pos gamestate.camera w h in  draw_rectangle x y size size
            (color_from_hsv ((float_of_int (b.ind*360)) /. (float_of_int qqty)) 0.9 1.);
          draw_text (string_of_int b.ind) (x+3) (y+3) 20 
          (if is_neight_preced b gamestate.objects then Color.raywhite else Color.black )   ;
      ) ((Player gamestate.player)::gamestate.objects);
    draw_text (Printf.sprintf "Plus que %d s" (gamestate.time/60)) 0 0 24 Color.raywhite;
    end_drawing ();

    let ww,a,s,d = gamestate.prevkeys in
    let nw, na, ns, nd = (is_key_down Key.W,is_key_down Key.A,is_key_down Key.S,is_key_down Key.D) in
    let objects = gamestate.objects in
    (*moved objects*)
    let mobjects = (
      if nw && not ww then (move (Player gamestate.player) (0,-1) objects)
      else if na && not a then (move (Player gamestate.player) (-1,0) objects)
      else if ns && not s then (move (Player gamestate.player) (0,1) objects)
      else if nd && not d then (move (Player gamestate.player) (1,0) objects)
      else ((Player gamestate.player)::gamestate.objects))
    in

    (* le joueur sera toujours la tête si il est déplacé*)
    let player = match List.hd mobjects with 
      Player p -> {pos = p.pos} 
      | _ -> gamestate.player 
    in

    let camera = let (xc, yc), (xp, yp) = gamestate.camera, gamestate.player.pos in 
      (xc + (xp * size - xc) / 7, yc + (yp * size - yc) / 7) 
    in

    let gamestate' = {
      objects = concat (List.tl mobjects) objects qqty;
      prevkeys = (nw, na, ns, nd);
      player = player; 
      camera = camera;
      time = (if gamestate.time = 0 then raise Perdu  else gamestate.time -1);
    }
    in loop gamestate' w h

let ophelie w h = loop (setup ()) w h
