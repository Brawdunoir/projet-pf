module type Iter =
sig
  type 'a t
  val vide : 'a t
  val cons : 'a -> 'a t -> 'a t
  val uncons : 'a t -> ('a * 'a t) option
  val unfold : ('s -> ('a * 's) option) -> 's -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val constant : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end


(* Module Flux implantant l'interface de flux Iter *)
(* à l'aide d'une structure de données paresseuse  *)
type 'a flux = Tick of ('a * 'a flux) option Lazy.t;;
module Flux : Iter with type 'a t = 'a flux =
  struct
    type 'a t = 'a flux = Tick of ('a * 'a t) option Lazy.t;;

    let vide = Tick (lazy None);;

    let cons t q = Tick (lazy (Some (t, q)));;

    let uncons (Tick flux) = Lazy.force flux;;
 
    let rec apply f x =
      Tick (lazy (
      match uncons f, uncons x with
      | None         , _             -> None
      | _            , None          -> None
      | Some (tf, qf), Some (tx, qx) -> Some (tf tx, apply qf qx)));;

    let rec unfold f e =
      Tick (lazy (
      match f e with
      | None         -> None
      | Some (t, e') -> Some (t, unfold f e')));;

    let rec filter p flux =
      Tick (lazy (
      match uncons flux with
      | None        -> None
      | Some (t, q) -> if p t then Some (t, filter p q)
                      else uncons (filter p q)));;
    
    let rec append flux1 flux2 =
      Tick (lazy (
      match uncons flux1 with
      | None          -> uncons flux2
      | Some (t1, q1) -> Some (t1, append q1 flux2)));;
    
    let constant c = unfold (fun () -> Some (c, ())) ();;
    (* implantation rapide mais inefficace de map *)
    let map f i = apply (constant f) i;;

    let map2 f i1 i2 = apply (apply (constant f) i1) i2;;
  end

module Chepa =
  struct
    (* à compléter/modifier *)
    type res =
      | ValidDone
      | InvalidDone
      | Fork of (unit -> res) * unit;;

    let prompt0 = Delimcc.new_prompt();;

    (* forsome_bool : unit -> bool *)
    (* forke de la même façon l'exécution courante *)
    (* l'exécution parente est VALIDE si et seulement *)
    (* si au moins une exécution fille l'est *)
    let forsome_bool exec = Delimcc.shift prompt0 (fun k -> Fork (k,exec))

    (* forall : 'a Flux.t -> 'a *)
    (* prend chaque élément du flux et forke l'exécution *)
    (* courante en autant d'élément du flux *)
    let rec forall flux =
      Tick (lazy (
        match Flux.uncons flux with
          | None -> failwith("Erreur")
          | Some(t, flux') -> match Flux.uncons flux' with
                              | None -> t
                              | _ -> let b = forall_bool in if b then t else forall flux'
      ))
end
