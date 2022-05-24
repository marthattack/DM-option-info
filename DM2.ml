type dict =
Node of bool*(char*dict)list;;

let aux1 = Node(true,[('f',Node(true,[]))])
let aux2 = Node(false,[('g',Node(false,[('y',Node(true,[]));('z',Node(true,[]))]));('m',Node(false,[]))])
let aux3 = Node(false, [('u', Node(true, [('z', Node(false,[]))]))])
let aux4 = Node(false,[('c',Node(false, [('a', Node(true,[]))]));('y',Node(true,[]))])
let aux5 = Node(true,[])
let ex = Node(false,[('a',aux1); ('g',aux2); ('h',aux3); ('i', aux4); ('m', aux5)]);;

(*Q1*)
let empty_dict = Node(false,[]);;

(*Q2*)
let is_leaf x = match x with
|Node(a,[]) -> true
|_ -> false;;

is_leaf (Node(false,[]));;
is_leaf (Node(false,['a',Node(true,[])]));;

(*Q3*)
let rec taille dico = match dico with
|Node(a,[]) -> if a = true then 1 else 0
|Node(a,b) -> if a = true then 1+(aux b) else (aux b)
and
aux liste = match liste with
|[] -> 0
|(a,b)::q -> (taille b) + (aux q);;

taille ex;;

(*Q4*)
let rec longueur dico = match dico with
|Node(a,[]) -> if a = true then 0 else -1
|Node(a,b) -> 1 + (aux2 b)
and 
aux2 liste = match liste with
|[] -> 0
|(a,b)::q -> max (longueur b) (aux2 q);;

longueur ex;;
longueur (Node(false,[]));;

(*Q5*)
let rec assoc_ord x l = match l with
|[] -> None
|(a,b)::q -> if a=x then (Some b) else( assoc_ord x q);;

assoc_ord 'c' [('a',1 ); ('c',2); ('m', 0)];;
assoc_ord 'c' [('a',1 ); ('m', 0)];;

(*Q6*)
let rec find_subtree c tree = match tree with
|Node(a,[]) -> None
|Node(a,b) -> aux3 c b
and 
aux3 x liste = match liste with
|[] -> None
|(a,b)::t -> if a=x then (Some b) else (aux3 x t);;

find_subtree 'c' (Node(true,[('a', Node(false,[])); ('c', Node(true, []))]));;
find_subtree 'c' (Node(true,[('a', Node(false,[])); ('d', Node(true, []))]));;

(*Q7*)
let mem tree word = 
let rec aux4 i tree word = match (find_subtree word.[i] tree) with
|None -> false
|Some (Node(a,b)) -> if a=true && i=((String.length word)-1) then true else if i=((String.length word)-1) then false else (aux4 (i+1) (Node(a,b)) word)
in aux4 0 tree word;;

mem ex "ggu";;

(*Q8*)
let rec add_string_aux dict s x = match dict with
|Node(false,[]) -> tldn [] s x
|Node(a,b) -> tldn b s x
and
tldn liste s x = match (find_subtree (s.[x]) (Node(false,liste))) with
|None -> (s.[x],(add_string_aux (Node(false,[])) s (x+1)))::liste
|Some b -> add_string_aux b s (x+1);;

(*Q8, essai 2*)
let rec add_string_aux dict s x = match (find_subtree (s.[x]) dict) with
|None -> (match dict with
		|Node(false,[]) -> Node(false,tldn [] (String.sub s x ((String.length string)-1)) 0)
		|Node(a,b) -> Node(a, tldn b s 0))
|Some b -> add_string_aux dict (String.sub s x ((String.length string)-1)) 0
and
tldn liste s x = match liste with
|[] -> (s.[x], add_string_aux (Node(false,[])) (String.sub s x ((String.length string)-1)) 0)
|_ -> [(0,add_string_aux (Node(false,[])) (String.sub s x ((String.length string)-1)) 0)]@liste;;































