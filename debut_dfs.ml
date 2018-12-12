


(* Une case est représentée par sa lettre et son numéro de case *)
type case = {lettr:string;num:int};;

(* [O E I
    L U L
	I T B] *)

let grille = [[{lettr="E";num=1};{lettr="U";num=4};{lettr="L";num=3}];	(*O*)
(*E*)			[{lettr="O";num=0};{lettr="L";num=3};{lettr="U";num=4};{lettr="L";num=5};{lettr="I";num=2}];
(*I*)				[{lettr="E";num=1};{lettr="U";num=4};{lettr="L";num=5}];
(*L*)					[{lettr="O";num=0};{lettr="E";num=1};{lettr="U";num=4};{lettr="T";num=7};{lettr="I";num=6}];
(*U*)						[{lettr="O";num=0};{lettr="E";num=1};{lettr="I";num=2};{lettr="L";num=3};{lettr="L";num=5};{lettr="T";num=7};{lettr="I";num=6};{lettr="B";num=8}];
(*L*)							[{lettr="I";num=2};{lettr="E";num=1};{lettr="U";num=4};{lettr="T";num=7};{lettr="B";num=8}];
(*I*)								[{lettr="L";num=3};{lettr="U";num=4};{lettr="T";num=7}];
(*T*)									[{lettr="I";num=6};{lettr="L";num=3};{lettr="U";num=4};{lettr="L";num=5};{lettr="B";num=8}];
(*B*)										[{lettr="L";num=5};{lettr="U";num=4};{lettr="T";num=7}]];;

				
				
let rec voisins = fun 
	(0,a::l)->a
	|(x,a::l)->voisins(x-1,l);;
	
voisins(4,grille);;
(*- : case list =
 [{lettr = "O"; num = 0}; {lettr = "E"; num = 1}; {lettr = "I"; num = 2};
  {lettr = "L"; num = 3}; {lettr = "L"; num = 5}; {lettr = "T"; num = 7};
  {lettr = "I"; num = 6}; {lettr = "B"; num = 8}]*)
	
(*inListe : 'a * 'a list -> bool = <fun>*)
let rec inListe = fun
	(a,b::l)->if a=b then true else inListe(a,l)
	|(_,[])->false;;
	
let c1={lettr="L";num=3};;
let c2={lettr="T";num=7};;

let rec premier = fun (x,l)->let a = voisins(x,l) in hd(a);;
(*premier : int * 'a list list -> 'a = <fun>*)
premier(0,grille);;
(*- : case = {lettr = "E"; num = 1}*)

inListe(c1,[{lettr="O";num=0};{lettr="L";num=3};{lettr="U";num=4};{lettr="L";num=5};{lettr="I";num=2}]);;
(*- : bool = true*)
inListe(c2,[{lettr="O";num=0};{lettr="L";num=3};{lettr="U";num=4};{lettr="L";num=5};{lettr="I";num=2}]);;
(*- : bool = false*)

let rec nonVisite = fun
	(c::l,v)-> if inListe(c,v) then nonVisite(l,v)
				else c
	|([],_)->(-1);;

nonVisite([{lettr="E";num=1};{lettr="U";num=4};{lettr="L";num=3}],[{lettr="E";num=1};{lettr="U";num=4}]);;
(* - : case = {lettr = "L"; num = 3} *)
nonVisite([{lettr="E";num=1};{lettr="U";num=4};{lettr="L";num=3}],[{lettr="E";num=1};{lettr="U";num=4};{lettr="L";num=3}]);;
(* - : case = {lettr = ""; num = -1} *)



let adjacence = fun
	0 -> [1;4;5]
	| 1 -> [0;2;4;5;6]
	| 2 -> [1;3;5;6;7]
	| 3 -> [2;6;7]
	| 4 -> [0;1;5;8;9]
	| 5 -> [0;1;2;4;6;8;9;10]
	| 6 -> [1;2;3;5;7;9;10;11]
	| 7 -> [2;3;6;10;11]
	| 8 -> [4;5;9;12;13]
	| 9 -> [4;5;6;8;10;12;13;14]
	| 10 -> [5;6;7;9;11;13;14;15]
	| 11 -> [6;7;10;14;15]
	| 12 -> [8;9;13]
	| 13 -> [8;9;10;12;14]
	| 14 -> [9;10;11;13;15]
	| 15 -> [10;11;14] ;;
	

let rec profond = fun
	(x,grille,v)->let a = voisins(x,grille) in let next=nonVisite(a,v) in if next.num=(-1) then [] else next.num::profond(next.num,grille,next::v);;
	
let rec profondF = fun
	(x,v)->let voisins = adjacence(x) in let n=nonVisite(voisins,v) in if n=(-1) then [] else n::profondF(n,n::v);;
	
let rec profondV = fun
	(x,v)->let voisins = adjacence(x) in if 