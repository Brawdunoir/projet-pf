module Chepa = (**A corriger le nom *)
    struct
     type res = 
            | ValidDone
            | InvalidDone;;
    let prompt0 = Delimcc.new_prompt();;
    (** miracle : unit -> 'a 
    Fonction qui interrompt l'exécution et la rend valide
    (Sans renvoyer de valeur)
    *)
    let miracle = 
        Delimcc.shift prompt0 (fun _ -> ValidDone);;
    (** miracle : unit -> 'a 
    Fonction qui interrompt l'exécution et la rend invalide
    (Sans renvoyer de valeur)
    *)
    let failure = 
        Delimcc.shift prompt0 (fun _ -> InvalidDone);;
end
