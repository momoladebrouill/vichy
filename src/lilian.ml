let size = 10.
let speed = 3
let qqty = 100
let dead_ray = 200.
let repulse_ray = 2500.
let id_ray = 5000.
let show_ray = 20000.
type boul = {pos: int*int; dir: int*int} 
type obj = 
  Player of boul
  | Bad of boul 
  | Good of boul

exception Perdu

type gamestate = {
  bads : obj list;
  player :  boul;
  good : boul;
  prevkeys : (int*int*int*int);
  time : int;
}

let gen_p w h = {pos = ((Random.int 20) * w / 20, (Random.int 20) * h / 20); dir = List.nth [(-1,0); (1,0);( 0,-1); (0,1)] (Random.int 3);} 

let setup w h =
  {
    player = {pos = ( 40, 40); dir = (0, 0)};
    good = (gen_p w h);
    bads = List.init 100 (fun _ -> Bad (gen_p w h));
    prevkeys = (0, 0, 0, 0);
    time = 0;
  }


let ($+) a b =
  let xa,ya = a in
  let xb,yb = b in
  (xa + xb, ya + yb)

let ($*) a b =
  let x, y = a in (x * b, y * b)

let ($/) a s =
  let xa, ya = a in
  let xs, ys = s in
  ((xa mod xs + xs) mod xs, (ya mod ys + ys) mod ys)

let dist a b =
  let (x, y), (x', y') = a.pos, b.pos in 
  (float_of_int (x - x')) ** 2. +. (float_of_int (y - y')) ** 2.

let int_of_bool b=
  if b then 1 else 0

let repulse b player =
  let _ = (player, b) in
  (0, 0)(*
  if dist b player > repulse_ray then (0, 0) else
  let (x, y), (xp, yp) = b.pos, player.pos in
  (if x - xp < 0 then -1 else 1), (if y - yp < 0 then -1 else 1)
*)
let rec loop gamestate w h =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    let objects = (Player gamestate.player)::(Good gamestate.good)::gamestate.bads in
    let player = gamestate.player in
    let good = gamestate.good in
    List.iter (fun obj -> 
      match obj with
        Player p -> let x, y = p.pos in draw_rectangle (x-5) (y-5) 10 10 Color.orange
      | Bad b -> let x, y = b.pos in draw_circle x y size 
        (let d = dist player b in
          if d < dead_ray && gamestate.time > 60 then raise Perdu
          else if d < id_ray then Color.red 
          else if d < show_ray then Color.white 
          else Color.black) 
      | Good b -> let x,y = b.pos in draw_circle x y size 
        (let d = dist player b in 
        if d < show_ray then Color.green else Color.black) 
    ) objects;
    end_drawing ();
    
    let nw, na, ns, nd = (
      int_of_bool(is_key_down Key.W),
      int_of_bool(is_key_down Key.A),
      int_of_bool(is_key_down Key.S),
      int_of_bool(is_key_down Key.D)
    ) in
        
    let bads' = List.map (fun obj ->
      match obj with Bad b -> Bad {pos = (repulse b player) $* 5 $+ b.pos $+ (b.dir $* 1) $/ (w, h);dir = b.dir;} | _ -> Good gamestate.good) objects in
      let good' = {pos = good.pos $+ (good.dir $* 2) $/ (w, h);dir = good.dir;} in
      let player' = {pos = (let x, y = player.pos in (x + (nd - na) * speed, y + (ns - nw) * speed)); dir = (69, 420)} in
      if dist player good > (dead_ray) then
        loop {
          bads = bads';
          good = good';
          player = player';
          prevkeys = (nw,na,ns,nd);
          time = (gamestate.time + 1) 
        } w h else () 

let lilian w h = loop (setup w h) w h
