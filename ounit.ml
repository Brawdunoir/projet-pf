open Flux

type res =
    | Valid
    | Invalid

(* Prompt de la librairie Delimcc *)
let p = Delimcc.new_prompt ()

let miracle () = Delimcc.shift p (fun _ -> Valid)

let failure () = Delimcc.shift p (fun _ -> Invalid)

let assumption predicat =
    if predicat () then
        Delimcc.shift p (fun k -> k ())
    else
        miracle ()

let assertion predicat =
    if predicat () then
        failure ()
    else
        Delimcc.shift p (fun k -> k ())

let forall_bool () =
    Delimcc.shift p (fun k ->
        match (k true, k false) with
        | (Valid, Valid) -> Valid
        | _ -> Invalid )

let forsome_bool () =
    Delimcc.shift p (fun k ->
        match (k true, k true) with
        | (Invalid, Invalid) -> Invalid
        | _ -> Valid)

let rec forall flux =
    match Flux.uncons flux with
    | Some(t, flux') ->
        (match Flux.uncons flux' with
        | None -> t
        | _ -> if forall_bool () then t else forall flux')
    | _ -> failure ()

let rec forsome flux =
    match Flux.uncons flux with
    | Some(t, flux') ->
        (match Flux.uncons flux' with
        | None -> miracle ()
        | _ -> if forsome_bool () then t else forsome flux')
    | _ -> failure ()

let rec foratleast n flux =
    match n, Flux.uncons flux with
    | 0 , _ -> miracle ()
    | _ , None -> failure ()
    | _ , Some(t, flux') -> match forall_bool () with
                            | true -> t; foratleast (n-1) flux'
                            | _ -> foratleast n flux'

let check prog =
    match Delimcc.push_prompt p (fun () -> prog (); Valid) with
    | Valid -> true
    | Invalid -> false
