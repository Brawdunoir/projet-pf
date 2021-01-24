let%test _ =
    let values = unfold (fun cpt -> if cpt > 50 then None else Some (cpt, cpt+1)) 2 in
    check (fun () ->
    let a = forall values in
    let b = forsome values in
    assumption (fun () -> a <> b);
    let r = premier a in
    let predicat = (fun () -> r || (a mod b = 0))
    in
    if predicat () then
        failure (Format.printf "%d,%d@." a b)
    else
        Delimcc.shift p (fun k -> k ()))