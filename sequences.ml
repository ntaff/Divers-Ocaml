(* Type séquence *)
type ’a seq =
	Nil
	| Cons of ’a * (unit -> ’a seq);;

(* type ’a list =
	[]
	| (::) of ’a * ’a list;; *)

(* Renvoie la liste des n premiers elements de xs *)
(* val take : int -> int list -> int list = <fun> *)
let rec take (n : int) (xs : int list) = match xs with
	a::l -> if n > 0 then a::(take (n-1) (l)) else []
	|[] -> [];;

(* TESTS *)
take 3 [1;2;3;4;5] ;; (* - : int list = [1; 2; 3] *)
take 7 [1;2;3;4;5] ;; (* - : int list = [1; 2; 3; 4; 5] *)

(********************************************************************************************)

(* Generation d'une liste *)
(* val from : int -> int list = <fun> *)
let rec from k = k::(from (k+1));;

(* Selection d'elements d'une sequence *)
(* val takeq : int -> 'a seq -> 'a list = <fun> *)
let rec takeq n = function
	Nil -> []
	| Cons(x, xq) -> if n = 0 then [] else x::(takeq (n-1) (xq()));;

(* Generation d'une sequence *)
(* val fromq : int -> int seq = <fun> *)
let rec fromq k = Cons (k, fun() -> fromq(k+1));;

(* TESTS *)
takeq 5 (fromq 3) ;; (* - : int list = [3; 4; 5; 6; 7] *)

(*******************************************************************************************)

(* Applique la fonction f a chaque element de la sequence xq *)
(* val mapq : ('a -> 'b) -> 'a seq -> 'b seq = <fun> *)
let rec mapq f = function
	Nil -> Nil
	|Cons(x, xq) -> Cons(f x, fun() -> mapq f (xq()));;
	
(* TESTS *)
takeq 4 (fromq 1) ;; (* - : int list = [1; 2; 3; 4] *)
takeq 4 (mapq (fun x -> x + 2) (fromq 1)) ;; (* - : int list = [3; 4; 5; 6] *)

(* Renvoie la sequence des elements de xq qui verifient le predicat p.  *)
(* val filterq : ('a -> bool) -> 'a seq -> 'a seq = <fun> *)
let rec filterq p = function
	Nil -> Nil
	|Cons(x, xq) -> if p x then Cons(x, fun() -> filterq p (xq())) else filterq p (xq());;

(* TESTS *)
takeq 6 (filterq (fun x -> x mod 2 = 0) (fromq 1)) ;; (* - : int list = [2; 4; 6; 8; 10; 12] *)
