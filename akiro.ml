module Chepa =
struct
	type res =
			| ValidDone
			| InvalidDone
	let prompt0 = Delimcc.new_prompt();;

	(** forall_bool : unit -> bool
	Fonction qui interrompt l'exécution et la rend valide
	(Sans renvoyer de valeur)
	*)
	let forall_bool () =
		Delimcc.shift prompt0 (fun k -> (
			match k true with
			| ValidDone -> k false
			| InvalidDone -> InvalidDone))

end
