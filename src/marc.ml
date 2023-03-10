(* Marc :  tour de Hanoï revisté en mode survival (dédicace au cours sur les piles)*)

let slabWidth = 20
let slabHeight = 22

exception Perdu

type slab = 
    Null 
  | Slab of {size: int; next: slab; color: Raylib.Color.t }

type tower = { 
  top : slab;
  index : int;
}

type gamestate = {
  towers : tower list;
  hold: slab;
  current : int;
  prevKeys : (bool*bool*bool*bool*bool);
  time : int;
  nextslab : (int*Raylib.Color.t);
  spawn_tempo : int;
  round: int;
}

let colors = [|
  Raylib.color_from_hsv 360. 1. 1.;
  Raylib.color_from_hsv 270. 1. 1.;
  Raylib.color_from_hsv 180. 1. 1.;
|]

let random_color () =
    colors.(Random.int 3)

let generate_tower n =
  let rec gen acc i =
    if i = 0 then acc
    else gen (Slab {size = i; next = acc; color = random_color () } ) (i-1)
  in
  gen Null (n)

let generate_towers () =
  let rec gen acc i =
    if i = 0 then acc
    else gen ({top = if i mod 2 = 0 then generate_tower 4 else Null; index = i-1}::acc) (i-1)
  in gen [] 9

let towerPos i w h = 
  ( (1 + (i mod 3)) * w / 4, (1 + (i / 3)) * h / 4 + h / 8)


let gen_slab () = (Random.int 4 + 1, random_color ()) 

let setup () =
  { 
    towers = generate_towers ();
    hold = Null;
    current = 0; 
    prevKeys = (false, false, false, false, false);
    time = 0;
    nextslab = (1,random_color ());
    spawn_tempo = 5*60;
    round = 20;
  }

let draw_slab x y b =
  match b with
    Null -> ()
  | Slab b ->
    Raylib.draw_rectangle (x - b.size * slabWidth) y (b.size * slabWidth * 2) slabHeight b.color 

let draw_tower top x y =
  let rec aux t x y = 
    match t with
    | Null -> y
    | Slab b -> let ny = (aux b.next x y) - slabHeight in draw_slab x ny (Slab b); ny
  in let _ = aux top x y in ()

let rec first_not_none l default =
  match l with
    [] -> default
  | t::q -> match t with 
    |None -> first_not_none q default 
    |Some s -> s 

let checkwin tow =
  let rec aux top last =
    match top, last with
    | (Null, Null) -> false
    | (Slab a, Null) -> a.size = 1 && aux a.next (Slab a)
    | (Null, Slab b) -> b.size = 4
    | (Slab a, Slab b) -> (a.size = b.size + 1 && b.color = a.color) && aux a.next (Slab a)
  in aux tow.top Null

let towerHeight tow =
  let rec aux top = 
    match top with
    | Null -> 0
    | Slab a -> 1 + aux a.next
  in aux tow.top


let rec loop gamestate w h =
  if Raylib.window_should_close () then Raylib.close_window () else

    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    let mess = "In yours hands:" in 
    draw_text mess (w/2-(measure_text mess 24)/2) (h/18) 24 Color.raywhite;
    draw_slab (w/2) (h/9) gamestate.hold;
    let mess = ("Next to arrive:")in 
    draw_text mess 0 (h/18) 20 Color.raywhite;
    let s,c = gamestate.nextslab in
    draw_slab (s*32) (h/9) (Slab {color=c;size=s;next=Null});
    let mess = "Rounds left: "^(string_of_int gamestate.round) in
    draw_text mess (w-(measure_text mess 24)) (h/18) 20 Color.raywhite;


    List.iteri (fun i tow-> 
        let x, y = towerPos i w h in
        draw_rectangle 
          (x-slabWidth+3) (y-slabHeight*6) 
          ((slabWidth-3)*2) (slabHeight*6) 
          (if gamestate.current = tow.index then Color.raywhite else Color.gray);
        draw_tower tow.top x y; 
      ) gamestate.towers;

    end_drawing ();

    let spawn = if gamestate.time mod gamestate.spawn_tempo = 0 then 4 else -1 in

    let up, right, down, left, space = gamestate.prevKeys in

    let current = 
      (if is_key_down Key.A && not left then 
         (((gamestate.current - 1) mod 9) + 9)
       else if is_key_down Key.D && not right then 
         (gamestate.current + 1) 
       else if is_key_down Key.S && not down then
         (gamestate.current + 3)
       else if is_key_down Key.W && not up then
         (((gamestate.current - 3) mod 9) + 9)
       else gamestate.current) mod 9
    in

    (* un peu de tirage de cheveux ici mais on n'a pas trouvé d'autre manière : *)
    
    (* liste des tours et du potentiel tower hold : (tower, None) si on dépile pas, (tower, Some slab) si on dépile*) 
    let towershold = 
      List.map (fun tow -> 
          if is_key_down Key.Space && not space && (current = tow.index) then 
            match gamestate.hold with
            | Null -> ({top = (match tow.top with Null -> Null | Slab b -> b.next); index = tow.index}, Some tow.top)
            | Slab b -> match tow.top with
                | Null -> ({top = Slab {size = b.size; next = tow.top; color = b.color}; index = tow.index}, Some Null)
                | Slab c -> if b.size > c.size then 
                  (tow, None) 
                else 
                  ({top = Slab {size = b.size ; next = tow.top ; color = b.color}; index = tow.index},Some Null)
          else (tow,None)
        ) gamestate.towers 
    in
    
    (* on récupère ainsi la nouvelle slab qui est hold*)
    let hold = first_not_none (List.map (fun (_, hold) -> hold) towershold) gamestate.hold in
    
    (* Update tranquillou des tours, en prenant donc le premier élément des duos de towershold*)
    let towers = List.map (fun (tow, _) ->
      if checkwin tow then 
        {top = Null; index = tow.index}
      else if tow.index = spawn then 
        {top = (let s,c = gamestate.nextslab in Slab {size = s; next = tow.top; color = c }); index = tow.index}
      else 
        tow
    ) towershold in

    let spawn_tempo, nextslab = if spawn <> -1
      then gamestate.spawn_tempo (*modifications possibles ici si on veut que le jeu s'accélère*),(gen_slab ())
      else gamestate.spawn_tempo, gamestate.nextslab
    in

    let state = List.fold_left (fun a b -> a || towerHeight b > 6) false gamestate.towers in
    if state then raise Perdu else
    if gamestate.round <= 0 then () else
    let gamestate' = { 
      towers = towers;
      current = current;
      prevKeys = (is_key_down Key.W, is_key_down Key.D, is_key_down Key.S, is_key_down Key.A, is_key_down Key.Space);
      hold = hold;
      time = gamestate.time + 1;
      spawn_tempo = spawn_tempo;
      nextslab = nextslab;
      round = gamestate.round - if spawn <> -1 then 1 else 0 ;
    } in
    loop gamestate' w h


let marc w h = loop (setup ()) w h
