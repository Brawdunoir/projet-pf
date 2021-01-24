(* Module de OUnit *)
open Flux
(* Résultat renvoyé par une exécution *)
type res

(* assumption : (unit -> bool) -> unit *)
(* Fonction permettant de continuer seulement les exécutions *)
(* vérifiant un prédicat. On déclare VALIDE les exécutions *)
(* ne vérifiant pas le prédicat. *)
(* Paramètre predicat : (unit -> bool), le prédicat *)
(* Resultat : rien *)
val assumption : (unit -> bool) -> unit

(* assertion : (unit -> bool) -> unit *)
(* Fonction permettant de continuer seulement les exécutions *)
(* vérifiant un prédicat. On déclare INVALIDE les exécutions *)
(* ne vérifiant pas le prédicat. *)
(* Paramètre predicat : (unit -> bool), le prédicat *)
(* Resultat : rien *)
val assertion : (unit -> bool) -> unit

(* miracle : unit -> 'a *)
(* Fonction qui interrompt l'exécution et la rend VALIDE *)
(* Paramètre : rien *)
(* Resultat : 'a*)
val miracle : unit -> res

(* failure : unit -> 'a *)
(* Fonction qui interrompt l'exécution et la rend INVALIDE *)
(* Paramètre : rien *)
(* Resultat : 'a*)
val failure : unit -> res

(* forall_bool : unit -> bool *)
(* forke l'exécution en deux versions différentes *)
(* dans lesquelles le booléen donné est différent *)
(* On retourne un resultat valide ssi les DEUX exécutions *)
(* filles sont valides *)
(* Paramètre : rien *)
(* Résultat : Valid si les deux éxécutions filles sont Valid, *)
(* Invalid sinon. *)
val forall_bool : unit -> bool

(* forsome_bool : unit -> bool *)
(* forke l'exécution en deux versions identiques*)
(* dans lesquelles le booléen donné est identique *)
(* On retourne un resultat valide ssi au moins une des deux exécutions *)
(* filles sont valides *)
(* Paramètre : rien *)
(* Résultat : Valid si les deux éxécutions filles sont Valid, *)
(* Invalid sinon. *)
val forsome_bool : unit -> bool

(* forall : 'a Flux.t -> 'a *)
(* forke l'exécution courante (généralisation du forall_bool) *)
(* en autant de versions qu'il y a d'éléments dans le flux *)
(* Paramètre flux : flux d'éléments de 'a qui vont être évalués après fork *)
(* Résultat : les éléments du flux avec les deux éxécutions filles valides*)
val forall : 'a Flux.t -> 'a

(* forsome : 'a Flux.t -> 'a *)
(* forke l'exécution courante (généralisation du forsome_bool) *)
(* en autant de versions identiques qu'il y a d'éléments dans le flux *)
(* Paramètre flux : flux d'éléments de 'a qui vont être évalués après fork *)
(* Résultat : les éléments du flux avec au moins une des éxécutions filles valide*)
val forsome : 'a Flux.t -> 'a

(* foratleast : int -> 'a Flux.t -> 'a *)
(* forke l'éxécution courante en autant de versions identiques *)
(* qu'il y a d'éléments dans le flux et l'exécution parente est *)
(* valide ssi au moins n éxécutions filles le sont *)
(* Paramètre : - n le nombre minimum d'éxécutions valides qu'on veut, *)
(*             - flux d'éléments de 'a qui vont être évalués après fork *)
(* Résultat : une liste de au moins n exécutions ou Invalid si on a moins de n *)
val foratleast : int -> 'a Flux.t -> 'a

(* check : (unit −> unit) −> bool *)
(* Fonction renvoyant le booléen représentant la validité de l'exécution *)
(* Paramètre : prog l'éxécution à valider *)
(* Résultat : un bool qui est la validité de l'exécution *)
val check : (unit -> unit) -> bool
