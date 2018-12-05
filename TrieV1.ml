(********************
Longueur d'une chaîne
*********************)
let longChaine= fun s->
    string_length s;;

(***************************
sélection du nième caractère
****************************)
let niemeCar = fun (n,s)-> 
   nth_char s (n-1);;

(****************************************************
selection de la sous chaine entre les indices n et m
****************************************************)
let sousChaine = fun (s,n,m) -> 
if m<n then ""
else sub_string s (n-1) (m-n+1);;

(****************************************************** 
tetec donne l'initiale d'un mot sous forme de caractère
*******************************************************)
let tetec= fun 
""-> failwith "La chaine est vide"
| s-> niemeCar(1,s);;

(****************************************************** 
tetes donne l'initiale d'un mot sous forme de chaine
*******************************************************)
let tetes= fun s-> string_of_char(tetec(s));;

(***************************************
 reste supprime l'initiale d'une chaine
 ***************************************)
let reste = fun 
""-> failwith"La chaine est vide"
| s-> sousChaine (s,2,longChaine(s));;


type Trie = Noeud of string*bool*Noeud list;;

let t = Noeud("",false,[Noeud("a",false,[Noeud("m",false,[Noeud("i",true,[])])]);Noeud("z",false,[Noeud("o",false,[Noeud("o",true,[])])])]);;

let rec cpt = fun
	(Noeud(a,b,c::l))->if b=true then 1+cpt(c) else cpt(c)
	|(Noeud(_,_,[]))->0;;