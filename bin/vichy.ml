let w = 750
let h = 500

let slabWidth = 10
let slabHeight = 14

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
  prevKeys : (bool*bool*bool*bool*bool)
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
    else gen ({top = generate_tower 4; index = i-1}::acc) (i-1)
  in gen [] 9

let towerPos i = 
  ( (1 + (i mod 3)) * w / 4, (1 + (i / 3)) * h / 4 + h / 8)

let setup () =
  (* obligé par vim ???*)
  Raylib.init_window w h "Vichy";
  Raylib.set_target_fps 60;
  { 
    towers = generate_towers ();
    hold = Null;
    current = 0; 
    prevKeys = (false, false, false, false, false);
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


let rec loop gamestate =
  if Raylib.window_should_close () then Raylib.close_window () else

    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    draw_slab (w/2) (h/9) gamestate.hold;
    List.iteri (fun i tow-> 
        let x, y = towerPos i in
        draw_rectangle 
          (x-slabWidth + 3) (y-slabHeight*6) 
          ((slabWidth - 3)*2) (slabHeight*6) 
          (if gamestate.current = tow.index then Color.gray else Color.raywhite);
        draw_tower tow.top x y; 
      ) gamestate.towers;

    end_drawing ();


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
            | Null -> ({top = (match tow.top with Null -> Null | Slab b -> b.next); index = tow.index},Some tow.top)
            | Slab b -> match tow.top with
                | Null -> ({top = Slab {size = b.size ; next = tow.top ; color = b.color}; index = tow.index},Some Null)
                | Slab c -> if b.size > c.size then 
                  (tow, None) 
                else 
                  ({top = Slab {size = b.size ; next = tow.top ; color = b.color}; index = tow.index},Some Null)
          else (tow,None)
        ) gamestate.towers 
    in

    let gamestate' = { 
      towers = List.map (fun (tow,_) -> tow) towershold;
      current = current;
      prevKeys = (is_key_down Key.Up, is_key_down Key.Right, is_key_down Key.Down, is_key_down Key.Left, is_key_down Key.Space);
      hold = first_not_none (List.map (fun (_,hold) -> hold) towershold) gamestate.hold;
    } in
    loop gamestate'


let vichy () = loop (setup ())
