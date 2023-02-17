let w = 950
let h = 700

let slabWidth = 20
let slabHeight = 22

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
  nextslab : (int*Raylib.Color.t)
}

let colors = [|
  Raylib.color_from_hsv 360. 1. 1.;
  Raylib.color_from_hsv 270. 1. 1.;
  Raylib.color_from_hsv 180. 1. 1.;
  Raylib.color_from_hsv 90. 1. 1.;
|]

let random_color () =
    colors.(Random.int 4)

let generate_tower n =
  let rec gen acc i =
    if i = 0 then acc
    else gen (Slab {size = i; next = acc; color = random_color () } ) (i-1)
  in
  gen Null n

let generate_towers () =
  let rec gen acc i =
    if i = 0 then acc
    else gen ({top = generate_tower ((Random.int 2) * 4); index = i-1}::acc) (i-1)
  in gen [] 9

let towerPos i = 
  ( (1 + (i mod 3)) * w / 4, (1 + (i / 3)) * h / 4 + h / 8)

let setup () =
  (* obligé par vim ???*)
  Raylib.init_window w h "Vichy";
  Raylib.set_target_fps 60;
  Random.self_init ();
  { 
    towers = generate_towers ();
    hold = Null;
    current = 0; 
    prevKeys = (false, false, false, false, false);
    time = 0;
    nextslab = (1,random_color ());
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
    (* Mon reuf ?? très sus *)
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

 
let rec loop gamestate =
  if Raylib.window_should_close () then Raylib.close_window () else

    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    let mess = "In yours hands:" in 
    draw_text mess (w/2-(measure_text mess 24)/2) (h/18) 24 Color.raywhite;
    draw_slab (w/2) (h/9) gamestate.hold;
    let mess = ("Next to arrive:"^(Printf.sprintf "%02d" ((360-(gamestate.time mod 360))/10))) in 
    draw_text mess 0 (h/18) 20 Color.raywhite;
    draw_slab ((measure_text mess 20)/2) (h/9) (let s,c=gamestate.nextslab in Slab {color=c;size=s;next=Null});


    List.iteri (fun i tow-> 
        let x, y = towerPos i in
        draw_rectangle 
          (x-slabWidth + 3) (y-slabHeight*6) 
          ((slabWidth - 3)*2) (slabHeight*6) 
          (if gamestate.current = tow.index then Color.raywhite else Color.gray);
        draw_tower tow.top x y; 
      ) gamestate.towers;

    end_drawing ();

    let spawn = if gamestate.time mod 360 = 0 then Random.int 9 else -1 in

    let up , right, down, left, space = gamestate.prevKeys in

    let current = 
      (if is_key_down Key.Left && not left then 
         (((gamestate.current - 1) mod 9) + 9)
       else if is_key_down Key.Right && not right then 
         (gamestate.current + 1) 
       else if is_key_down Key.Down && not down then
         (gamestate.current +3)
       else if is_key_down Key.Up && not up then
         (((gamestate.current - 3)mod 9) +9)
       else gamestate.current) mod 9
    in

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

    let hold = first_not_none (List.map (fun (_, hold) -> hold) towershold) gamestate.hold in
    let towers = List.map (fun (tow, _) ->
      if checkwin tow then 
        {top = Null; index = tow.index} 
      else if tow.index = spawn then 
        {top = (let s,c = gamestate.nextslab in Slab {size = s; next = tow.top; color = c }); index = tow.index}
      else 
        tow
    ) towershold in

    
    let gamestate' = { 
      towers = towers;
      current = current;
      prevKeys = (is_key_down Key.Up, is_key_down Key.Right, is_key_down Key.Down, is_key_down Key.Left, is_key_down Key.Space);
      hold = hold;
      time = gamestate.time + 1;
      nextslab = if gamestate.time mod 360 = 0 then (Random.int 4 + 1, random_color ()) else gamestate.nextslab;
    } in
    loop gamestate'


let vichy () = loop (setup ())
