open Utilities
open Ounit
open Flux


let%test "assertion" =
    check (fun () -> assertion (fun () -> true))

let%test "assertion_false" =
    check (fun () ->
        assertion (fun () -> false)) = false


let%test "forall" =
    check (fun () ->
        let x = forall (range 1 10) in
        assertion (fun () -> x > 0))

let%test "forall_false" =
    check (fun () ->
        let x = forall (range 1 10) in
        assertion (fun () -> x < 9)) = false


let%test "forsome" =
    check (fun () ->
        let x = forsome (range 1 10) in
        assertion (fun () -> x > 8))

let%test "forsome_false" =
    check (fun () ->
        let x = forsome (range 1 10) in
        assertion (fun () -> x > 10)) = false


let%test "foratleast" =
    check (fun () ->
        let x = foratleast 3 (range 1 10) in
        assertion (fun () -> x > 7))

let%test "foratleast_false" =
    check (fun () ->
        let x = foratleast 4 (range 1 10) in
        assertion (fun () -> x > 7)) = false


let%test "exemple 1" =
    let values = (range 2 50) in
    check (fun () ->
    let a = forall values in
    let b = forsome values in
    assumption (fun () -> a <> b);
    let r = premier a in
    let predicat = (fun () -> r || (a mod b = 0))
    in assertion predicat)
