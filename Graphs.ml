include "Sequences.ml";;

(* Type graphe, 'a est le type des noeuds et 'b le type des couts *)
type (’a, ’b) graph = (’a * ’a * ’b) list;;

let graph1 = [("a", "b", 2); ("a", "c", 4); ("b", "c", 1);("d", "b", 3); ("b", "d", 3); ("c", "d", 1)];;

(* 'a est le noeud a atteindre, 'a list est la liste des noeuds parcourus (en ordre inverse) et 'b est le cout *)
type (’a, ’b) state = (’a * ’a list * ’b);;

let (etat1: (string, int) state) = ("d",["c";"b";"a"],3);;
let (etat2: (string, int) state) = ("d",["b";"a"],2);;

(* Parcours un graphe en profondeur *)
(* val depthfirst : ('a -> 'a list) -> ('a -> bool) -> 'a -> 'a seq = <fun> *)
let depthfirst next sol x =
	let rec dfs = function
	[] -> Nil
	| y :: ys -> if sol y then Cons(y, fun () -> dfs (next y @ ys)) else dfs (next y @ ys)
							in dfs [x];;
		
(* Parcours un graphe en largeur *)	
(* val breadthfirst : ('a -> 'a list) -> ('a -> bool) -> 'a -> 'a seq = <fun> *)	
let breadthfirst next sol x =
	let rec bfs = function
	[] -> Nil
	| y :: ys -> if sol y then Cons(y, fun () -> bfs (ys @ next y)) else bfs (ys @ next y)
							in bfs [x];;
					
(***********************************************************************************************)

(* Teste si un etat est une solution *)
(* val sol_state : ('a, 'b) state -> bool = <fun> *)
let rec sol_state (e : ('a, 'b) state) = match e with
	(a,b::l,_) -> b = a;;

(* Prend un graphe, un etat et calcule la liste des successeurs de cet etat *)
(* val next_states : ('a, int) graph -> ('a, int) state -> ('a * 'a * int) list = <fun> *)
let rec next_states (g : ('a, int) graph) (etat : ('a, int) state) = match etat with
	(dep,(n::l),tcout)-> (match g with
							(n1,n2,cout)::li  -> if n1 = n then (dep,n2::n::l,(cout+tcout):('a,'b)state)::(next_states li etat) else (next_states li etat)
							| [] -> [])
	|_ -> [];;

next_states graph1 etat1;; (* - : (string * string list * int) state list = [("d" , ["d";"c";"b";"a"], 4)]*)
next_states graph1 etat2;; (* - : (string * string list * int) state list = [("d" , ["d";"c";"b";"a"], 4) ; ("d", ["d" ; "b" ; "a"], 5)] *)

(* val init_state 'a -> 'a -> ('a, int) state = <fun> *)
let init_state (n: 'a) (n1: 'a) = match n, n1 with
	a,b -> ((b,[a],0): ('a,'b) state);;
	
init_state "a" "d";; (* - : (string, int) state = ("d", ["a"], 0) *)
