let w = 500
let h = 500
let size = 50
let thick = 10

type dalle = None | Dal of {size:int;under:dalle}

type tower = { 
    qqty : int;
    top : dalle;
}

type gamestate = {
    towers : tower list;
    selected : int
}

let generate_tower n s=
    let rec gen acc i s =
        if i=0 then acc
        else let ns = s-10 in gen (Dal {size=ns;under = acc}) (i-1) ns
    in gen None n s


let setup () =
    Raylib.init_window w h "Vichy";
    Raylib.set_target_fps 60;
    { towers = [{qqty=2;top=generate_tower 5 size}]; selected = 0}

let rec draw_tower top  x y =
    let open Raylib in
    match top with
    | None -> ()
    | Dal b-> draw_rectangle (x-b.size) y (b.size*2) thick Color.red; 
              draw_tower b.under x (y+thick)

let rec loop gamestate =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
        let open Raylib in
        begin_drawing ();
        clear_background Color.black;

        List.iter (fun tow -> draw_tower tow.top (w/2 - tow.qqty*thick) (h/2)
        ) gamestate.towers;

        end_drawing ();
    loop gamestate
let marc () = loop (setup ())
