(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* Question 1 *)
(* string list -> string list *)
fun only_capitals xs = 
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

(* Question 2 *)
(* string list -> string *)
fun longest_string1 xs =
    foldl (fn(x,y) => if String.size(x) > String.size(y) then x else y) "" xs

(* Question 3 *)
(* string list -> string *)
fun longest_string2 xs =
    foldl (fn(x,y) => if String.size(x) >= String.size(y) then x else y) "" xs

(* Question 4 *)
(* (int * int -> bool) -> string list -> string *)
fun longest_string_helper f xs =
    foldl (fn(x,y) => if f(String.size(x),String.size(y)) then x else y) "" xs
(* string list -> string *)
val longest_string3 = longest_string_helper (fn(x,y) => x > y)
(* string list -> string *)
val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

(* Question 5 *)
(* string list -> string *)
val longest_capitalized =  longest_string1 o only_capitals

(* Question 6 *)
(* string -> string *)
val rev_string = String.implode o rev o String.explode

(* Question 7 *)
(* ('a -> 'b option) -> 'a list -> 'b *)
fun first_answer f lst = 
    case lst of
	[] => raise NoAnswer
      | x::xs => case f x of
		     SOME y => y
		   | NONE => first_answer f xs

(* Question 8 *)
(* ('a -> 'b list option) -> 'a list -> 'b list option *)
fun all_answers f lst =
    let
	fun all_answers_helper(acc,xs) = 
	    case xs of
		[] => SOME acc
	      | y::ys  => case f y of
			      SOME z => all_answers_helper(acc@z,ys)
			    | NONE => NONE
    in
	all_answers_helper([],lst)
    end

(*  Question 9 *)
(* pattern -> int *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)
(* pattern -> int *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size
(*string * pattern -> int *)
fun count_some_var (s,p) =
    g (fn _ => 0) (fn x => if x = s then 1 else 0) p

(* Question 10 *)
(* pattern -> bool *)
fun check_pat p =
    let
	fun variables_string_list p =
	    case p of
		Variable x => [x]
	      | TupleP ps => foldl (fn(pat,acc) => acc@variables_string_list(pat)) [] ps
	      | _ => []
	fun has_repeats lst =
	    case lst of
		[] => false
		   | x::xs => List.exists (fn y => y = x) xs orelse has_repeats xs
    in
	(not o has_repeats o variables_string_list) p
    end

(* Question 11 *)
(* valu * pattern -> (string * valu) list option *)
fun match (v,p) =
    case (v,p) of
	(_,Wildcard) => SOME []
      | (x,Variable s) => SOME [(s,x)]
      | (Unit, UnitP) => SOME []
      | (Const cv,ConstP cp) => if cv = cp then SOME [] else NONE
      | (Tuple tv,TupleP tp) => if List.length tv = List.length tp 
				then all_answers (fn (vs,ps) => match(vs,ps)) (ListPair.zip(tv,tp))
				else NONE
     | (Constructor(sv,vv),ConstructorP(sp,pp)) =>  if sv = sp then match(vv,pp) else NONE
     | (_,_) => NONE

(* Question 12 *)
(* valu -> pattern list -> (string * valu) list option *)
fun first_match v ps = 
    SOME(first_answer(fn p => match(v,p)) ps)
    handle NoAnswer => NONE
