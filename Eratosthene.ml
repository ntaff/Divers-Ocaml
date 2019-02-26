include "Sequences.ml";;

(* Implementation du crible d'Eratosthene *)
(* val sieve : int seq -> int seq = <fun> *)
let rec sieve = function
	Nil -> Nil
	|Cons(p,rest) -> Cons(p, fun() -> sieve(filterq (fun x -> x mod p != 0)(rest())));;

(* val primes : int seq = Cons (2, <fun>) *)	
let primes = sieve (fromq 2);;

(* TESTS *)
takeq 12 primes ;; (* - : int list = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37] *)
