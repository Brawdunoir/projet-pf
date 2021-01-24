open Utilities
open Flux
open Ounit


let%test "exemple 1" =
    let values = (range 2 50) in
    check (fun () ->
    let a = forall values in
    let b = forsome values in
    assumption (fun () -> a <> b);
    let r = premier a in
    let predicat = (fun () -> r || (a mod b = 0))
    in assertion predicat)
