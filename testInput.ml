
let longChaine= fun s->
    string_length s;;

let niemeCar = fun (n,s)-> 
   nth_char s (n-1);;


let sousChaine = fun (s,n,m) -> 
if m<n then ""
else sub_string s (n-1) (m-n+1);;


let tetec= fun 
""-> failwith "La chaine est vide"
| s-> niemeCar(1,s);;

(*--------------------------------------------------------*)

let rec liste = fun f -> 
		try let ligne = input_line f in ligne::liste(f) 
		with End_of_file -> [];;

let file = "C:\Users\pedago\Downloads\Ruzzle-master\dico.txt";;
let fic = open_in file;;
liste(fic);;

let rec inListe = fun
	(a,b::l)-> if a=b then true else inListe(a,l)
	|(_,[])->false;;
	
	
let rec longueur = fun
	a::l->1+longueur(l)
	|[]->0;;
	
type table={clé : char ; valeur : string list};;


let rec charToListe = fun
	(a,b::l)->if a=tetec(b) then b::charToListe(a,l) else charToListe(a,l)
	|(_,[])->[];;


	
let rec construct = fun
	(a::l)->(a,[])::construct(l)
	|[]->[];;
	
let rec find =

	
let rec hash = fun
	(a::l,dico)-> let mots_a = charToListe(a,dico) in 
		(a,mots_a)::hash(l,dico)
	|([],_)->[];;
	
hash([`o`;`n`;`c`;`b`],liste);;
	
	
