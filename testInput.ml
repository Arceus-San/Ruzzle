let file = "dico.txt";;

let ouvrir = fun x-> let fic = open_in x;;

let lire = fun f-> input_file f;;
						
let rec liremot = fun
	l->lire(l)::liremot(l)
	|End_of_file ->[];;