let rec sum_to_ acc x=
if x<=0 then acc else sum_to_ (acc+x) (x-1);;

let sum_to=sum_to_ 0;;

let rev lst=
	let rec rev_aux acc l=
		match l with
		| [] -> acc
		| h::t -> rev_aux (h::acc) t
	in
	rev_aux [] lst;;

let concat lhs rhs=
	let rec concat_ restLhs acc=
		match restLhs with
		| [] -> acc
		| h::t -> h::(concat_ t acc)
	in concat_ lhs rhs;;

let flatten lst=
	let rec flatten_ l acc=
		match l with
		| [] -> acc
		| h::t -> concat h (flatten_ t acc)
	in flatten_ lst [];;

let my_map f lst=
	let rec map_aux acc l=
		match l with
		| [] -> acc
		| h::t -> map_aux ((f h)::acc) t
	in
	rev(map_aux [] lst);;

type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree;;

let rec depth btr=
	match btr with
	| Empty -> 0
	| Node(_,lhs,rhs) -> 1+max (depth lhs) (depth rhs);;

let rec sum_tree btr=
	match btr with
	| Empty -> 0
	| Node(v,lhs,rhs) -> v+(sum_tree lhs)+(sum_tree rhs);;

let rec map_tree f btr=
	match btr with
	| Empty -> Empty
	| Node(v,lhs,rhs) -> Node(f v,map_tree f lhs,map_tree f rhs);;

let rec is_balanced btr=
	let rec is_balanced_aux btr=
		match btr with
		| Empty -> (0,true)
		| Node(v,lhs,rhs) -> let (lhs_height,lhs_acc)=is_balanced_aux lhs in 
												let (rhs_height,rhs_acc)=is_balanced_aux rhs in 
												(max (1+lhs_height) (1+rhs_height),lhs_acc && rhs_acc && (abs (lhs_height-rhs_height) <= 1))
	in let (_,result)=is_balanced_aux btr
  in result;;

type expr =
| Const of int
| Add of expr * expr
| Sub of expr * expr
| Mul of expr * expr;;

let eval expr=
	let rec eval_ expr=
		match expr with
		| Const(v) -> Const(v)
		| Add(Const(lhs),Const(rhs)) -> Const(lhs+rhs)
		| Add(lhs,rhs) -> eval_ (Add((eval_ lhs),(eval_ rhs)))
		| Sub(Const(lhs),Const(rhs)) -> Const(lhs-rhs)
		| Sub(lhs,rhs) -> eval_ (Sub((eval_ lhs),(eval_ rhs)))
		| Mul(Const(lhs),Const(rhs)) -> Const(lhs*rhs)
		| Mul(lhs,rhs) -> eval_ (Mul((eval_ lhs),(eval_ rhs)))
  in match (eval_ expr) with
									| Const(v) -> v
									| _ -> raise Exit;;
