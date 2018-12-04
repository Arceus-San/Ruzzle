(* Renvoie sous forme de liste les mots contenus dans un fichier *)
let rec liste = fun f -> 
		try let ligne = input_line f in ligne::liste(f) 
		with End_of_file -> [];;

let niemeCar = fun (n,s)-> 
   nth_char s (n-1);;

let tetec= fun 
""-> failwith "La chaine est vide"
| s-> niemeCar(1,s);;


(* Vérifie si un mot est dans une liste *)
let rec inListe = fun
	(a,b::l)-> if a=b then true else inListe(a,l)
	|(_,[])->false;;
	
(* Compte la taille de la liste *)
let rec longueur = fun
	(a::l)->1+longueur(l)
	|[]->0;;
	
type table={cle : char ; valeur : string list};;


(* Renvoie une liste composée de tous les mots commençants par une charactère *)
let rec charToListe = fun
	(a,b::l)->if a=tetec(b) then b::charToListe(a,l) else charToListe(a,l)
	|(_,[])->[];;


	
let rec construct = fun
	(a::l)->(a,[])::construct(l)
	|[]->[];;
	

(* Construit notre table de Hachage sous forme d'une liste de couples (char*string list) *)
let rec hash = fun
	(a::l,dico)-> let mots_a = charToListe(a,dico) in 
		{cle=a;valeur=mots_a}::hash(l,dico)
	|([],_)->[];;


(* Calcule le nombre de mots dans notre table de hachage *)
let rec nbDeMots = fun
	(a::l)->longueur(a.valeur)+nbDeMots(l)
	|[]->0;;

let rec find = fun 
	(a::l,mot)->if inListe(mot,a.valeur) then true else find(l,mot)
	|([],_)->false;;

let alpha = [`a`; `b`; `c`; `d`; `e`; `f`; `g`; `h`; `i`; `j`; `k`; `l`; `m`; `n`; `o`; `p`; `q`; `r`; `s`; `t`; `u`; `v`; `w`; `x`; `y`; `z`];;

let t1=Sys__time();;
let file = "/Users/Maxime/Downloads/dico100.txt";;
let fic = open_in file;;
let dico = liste(fic);;
let t2=Sys__time();;

let t3=Sys__time();;
inListe("world",dico);;
longueur(dico);;
let t4=Sys__time();;


let hachage = hash(alpha,dico);;
let t5=Sys__time();;
find(hachage,"world");;
nbDeMots(hachage);;
let t6=Sys__time();;


let tempsConstructListe = t2-.t1;;
let tempsRechercheListe = t4-.t3;;
let tempsRechercheTable = t6-.t5;;
	