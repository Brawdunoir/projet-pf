    (* abs : int -> int *)
    (* renvoie la valeur absolue d'un entier relatif *)
    (* Paramètre : x entier *)
    (* Postcondition : le résultat est positif *)
    let abs x = if x <= 0 then -x else x
    
    let%test _ = abs 3 = 3
    let%test _ = abs (-3) = 3
    let%test _ = abs 0 = 0

    (** pgcd : int -> int -> int
    Fonction qui calcule le plus grand diviseur commun des deux paramètres 
    Paramètres a b : deux entiers
    Précondition : a > 0 et b > 0
    Résultat : Plus Grand Diviseur Commun des deux paramètres
    *)
    let pgcd a b =
    let rec pgcd_rec a b =
        if a = b then a
        else if a > b then pgcd_rec (a-b) b
        else pgcd_rec a (b-a)
    in
    pgcd_rec (abs a) (abs b)

    let%test _ = pgcd 3 3 = 3
    let%test _ = pgcd 7 (-9) = 1
    let%test _ = pgcd (-38) (-24) = 2
    let%test _ = pgcd 20 50 = 10

    let premier n =
    let rec non_diviseur d =
        d * d > (abs n) || ((abs n) mod d <> 0 && non_diviseur (d+1))
    in
    (abs n) <> 1 && non_diviseur 2

    let%test _ = premier 7 = true
    let%test _ = premier 4 = false
    let%test _ = premier (-31) = true

    (* SIGNATURE *)
    let range a b =
        unfold (fun cpt -> if cpt > b then None else Some (cpt, cpt + 1)) a
    (* TEST *)