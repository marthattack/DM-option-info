(*Q1 Ex1*)
let cons a p =
if a = 0. && p = [] then p else a::p;;

let rec add p q = match (p,q) with
|a::r, b::s -> (a +. b)::(add r s)
|[], q -> q
|p, [] -> p;;

let a = [1.;2.;3.;4.;5.;6.;7.;8.;9.;0.];;
let b = [9.;8.;7.;6.;5.;4.;3.;2.;1.;0.];;

add a b;;

(*Q2*)
let rec mulscal a b= match a with
|t::q -> cons (b*.t) (mulscal q b)
|[] -> [];;

let c = [1.;1.;1.;1.];;

mulscal c 4.;;

let rec sub p q = match (p,q) with
|a::r, b::s -> cons (a -. b) (sub r s)
|[], q -> q
|p, [] -> p;;

sub a b;;


let rec mul p q = match (p,q) with
|a::b,q -> add (mulscal q a) (mul b (0.::q))
|[],q -> p;;

let rec eval a b = match a with
|t::q -> t +. b*.(eval q b)
|[] -> 0.;;

eval [1.;2.;3.] 2.;;

(* au d�but d=0, p=1, a=2*)
let rec hanoi n d p a= match n with
|0 -> []
|_ -> (hanoi (n-1) d a p)@[(d,a)]@(hanoi (n-1) p d a)
;;
hanoi 2 0 1 2;;

(*Ex2 Q3*)
type paire = 
|Triple of int*int*int
|Paire of int*int 
|Triple2 of int*int*int;;


