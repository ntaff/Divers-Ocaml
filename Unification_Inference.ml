(*"*********************************************************)
(********************* Nicolas *****************************)
(******************* Taffoureau ****************************)
(******************** 12/02/2019 ***************************)
(***********************************************************)

(* ----------------------- Mise en place -------------------- *)

(* Expressions du langage *)
type expr = 
    Const of string
  | Var of string
  | Func of string * expr
  | App of expr * expr
;;

(* Exemples *)
let t1 =  Func ("f", Func ("x", App (Var "f", Var "x")));;
let t2 =  Func ("x", App (Var "f", App ( Var "f", Var "x")));;
let t3 = Func("f", App(Var "f",  App(Var "f", Const "ic1")));;
let t4 = Func("f", App(Var "f",  App(Var "f", Const "bc1")));;
let t5 = Func("f", Func("g", Func("h",(Func ("a",  
    App(Var "h", (App(App (Var "f", App(Var "g", Var "h")), (App(Var "h", Var "a")))))))))) ;;

			(* Fonction permettant d'afficher une expression *)
			(* val afficherExp : expr -> string = <fun> *)
	   		let rec afficherExp exp = match exp with
				Const (c) -> c 									
				|Var (x) -> x
				|Func (a,b) -> "Fun "^a^" -> " ^ (afficherExp b)
				|App(a,b) -> (afficherExp a) ^ " ( "^(afficherExp b)^" )" ;;	   
			
			(* TESTS *)
			afficherExp(t1);; (* ----> - : string = "Fun f -> Fun x -> f ( x )" *)
			afficherExp(t2);; (* ----> - : string = "Fun x -> f ( f ( x ) )" *)
			afficherExp(t3);; (* ----> - : string = "Fun f -> f ( f ( ic1 ) )" *)
			afficherExp(t4);; (* ----> - : - : string = "Fun f -> f ( f ( bc1 ) )" *) 
			afficherExp(t5);; (* ----> - : string = "Fun f -> Fun g -> Fun h -> Fun a -> h ( f ( g ( h ) ) ( h ( a ) ) )" *)



(* Type pour les types *)
type tp = 
	ConstT of string
	| VarT of string  
	| FunT of tp * tp ;;

			(* Fonction permettant d'afficher un type *)
			let rec afficheType exp = match exp with
				ConstT (c) -> c 									
				|VarT (x) -> x
				|FunT (a,b) -> afficheType(a) ^ " -> "  ^(afficheType b);;
				
				(* TESTS *)
				let type1 =  FunT (ConstT "int", FunT (ConstT "bool", ConstT "int"));; (* Exemple de type *)
				afficheType(type1);; (* ----> - : string = "int -> bool -> int" *)

(* Environnement *)
type envt = (string * tp) list ;;

(* Listes d'associations *)
type 'a option =
	None
	| Some of 'a ;;
	
let rec cherche x = function
	| [] -> None
	| (a,b) :: q -> if x = a then Some b else cherche x q ;;
	

(* Fonction tpVariable, renvois si le type de la variable si elle est dans l'environnement e *)
(* val tpvariable : envt -> string -> tp = <fun> *)
let tpVariable (e : envt) (s : string) = match (cherche s e) with
	None -> failwith ("La variable n'est pas dans l'environnement courant")
	|Some a -> a;;

	(* TESTS *)
	let (env1 :envt) = [("v", ConstT "int")];;
	tpVariable env1 "v";; (* ----> - : tp = ConstT "int" *)
	tpVariable env1 "w";; (* ----> Exception: Failure "La variable n'est pas dans l'environnement courant". *)

(* Fonction tpConstante qui donne le type d’une constante. On suppose 
que notre langage ici contient deux constantes de type int, notées
"ic1" et "ic2" et deux constantes de type bool, notées "bc1" et "bc2" *)

let tpConstante (s : string) = match s with
	s when (s = "ic1" || s = "ic2") -> ConstT "Int"
	|s when (s = "bc1" || s = "bc2") -> ConstT "Bool"
	| s -> failwith ("Constante non reconnue");;

	(* TESTS *)
	tpConstante "ic1";; (* ----> - : tp = ConstT "Int" *)
	tpConstante "bc2";; (* ----> - : tp = ConstT "Bool" *)
	tpConstante "ibm3";; (* ----> Exception: Failure "Constante non reconnue". *)
	
	
(* Créer de nouvelles variables de types *)

let nouvelleVarTp (compteur : int) (s : string option) = match s with
	Some b -> (b^string_of_int(compteur)), compteur+1
	|None -> ("a"^string_of_int(compteur)), compteur+1;;

	(* TESTS *)
	nouvelleVarTp 3 None;; (* ----> - : string * int = ("a3", 4) *)
	nouvelleVarTp 3 (Some "v");; (* ----> - : string * int = ("v3", 4) *)
