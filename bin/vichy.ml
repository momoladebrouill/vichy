let w = 750
let h = 500

let slabWidth = 10
let slabHeight = 14

type slab = None | Slab of {size:int; next: slab; color: Raylib.Color.t }

type tower = { 
  top : slab;
  index: int;
}

type gamestate = {
  towers : tower list;
  hold: slab;
  current : int;
  prevKeys : (bool*bool*bool*bool*bool)
}

let generate_tower n =
  (* n quantité s taille du plus gros*)
  let rec gen acc i =
    if i = 0 then acc
    else gen (Slab {size = i; next = acc; color = match (Random.int 3) with | 0 -> Raylib.Color.red | 1 -> Raylib.Color.blue | 2 -> Raylib.Color.green | _ -> failwith "no"}) (i-1)
  in
  gen None n

let generate_towers ()=
  let rec gen acc i =
    if i = 0 then acc
    else gen ({top = generate_tower 3; index = i-1}::acc) (i-1)
  in gen [] 9

let towerPos i = 
  ( (1 + (i mod 3)) * w / 4, (1 + (i / 3)) * h / 4)

let setup () =
  (* obligé par vim ???*)
  Raylib.init_window w h "Vichy";
  Raylib.set_target_fps 60;
  { 
    towers = generate_towers ();
    hold = None;
    current = 0; prevKeys = (false, false, false, false, false);
  }

let draw_tower top x y =
  let rec aux t x y = 
    match t with
    | None -> y
    | Slab b -> let ny = (aux b.next x y) - slabHeight in
      Raylib.draw_rectangle (x - b.size * slabWidth) ny (b.size * slabWidth * 2) slabHeight b.color; 
      ny
  in let _ = aux top x y in ()


let rec loop gamestate =
  if Raylib.window_should_close () then Raylib.close_window () else

    let open Raylib in
    begin_drawing ();
    clear_background Color.black;

    List.iteri (fun i tow-> 
      let a, b = towerPos i in
        draw_rectangle (a-slabWidth + 3) (b-slabHeight*6) ((slabWidth - 3)*2) (slabHeight*6) (if gamestate.current = tow.index then Color.yellow else Color.raywhite);
        draw_tower tow.top a b; 
        ()
      ) gamestate.towers;

    end_drawing ();


    let _ , b, _, d, e = gamestate.prevKeys in

    let current = 
    let open Raylib in
    if is_key_down Key.Left && not d then (((gamestate.current - 1) mod 9) + 9) mod 9
    else if is_key_down Key.Right && not b then ((gamestate.current + 1) mod 9)
    else gamestate.current in
 
    let towers = List.map (fun tow -> 
      if is_key_down Key.Space && not e && current = tow.index then 
        {top = (match tow.top with None -> None | Slab b -> b.next); index = tow.index} 
      else 
        tow
    ) gamestate.towers in

      

    let gamestate' = { 
      towers = towers;
      current = current;
      prevKeys = (false, is_key_down Key.Right, false, is_key_down Key.Left, is_key_down Key.Space);
      hold = None
    } in
    loop gamestate'


let vichy () = loop (setup ())
