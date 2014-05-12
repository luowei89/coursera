(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* question 1a *)
fun all_except_option (str,str_lst) = 
    case str_lst of
	[] => NONE
     |  x::xs => case (same_string(x,str),all_except_option(str,xs)) of
		    (true,_) => SOME xs
		  | (false,NONE) => NONE
		  | (false,SOME lst) => SOME (x::lst)

(* question 1b *)
fun get_substitutions1 (str_lst_lst,str) =
    case str_lst_lst of
	[] => []
      | ls::lss => case all_except_option(str,ls) of
		       NONE => get_substitutions1(lss,str)
		     | SOME lst => lst@get_substitutions1(lss,str)

(* question 1c *)
fun get_substitutions2 (str_lst_lst,str) =
    let 
	fun get_substitutions_helper(sll,acc) =
	    case sll of
		[] => acc
	      | sl::sls => case all_except_option (str,sl) of
			      NONE => get_substitutions_helper(sls,acc)
			    | SOME lst => get_substitutions_helper(sls,acc@lst) 
    in
	get_substitutions_helper(str_lst_lst,[])
    end

(* question 1d *)
fun similar_names (str_lst_lst,{first=f, middle=m, last=l}) =
    let
	fun  append_names(str_lst,full_name_lst) =
	     case str_lst of
		 [] => full_name_lst
	       | s::ss => append_names(ss,full_name_lst@[{first=s,middle=m,last=l}])
    in
	append_names(get_substitutions2(str_lst_lst,f),[{first=f, middle=m, last=l}])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* question 2a *)
fun card_color card =
    case card of
	(Clubs,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red
      | (Spades,_) => Black
			  
(* question 2b *)
fun card_value card =
    case card of
	(_,Num x) => x
      | (_,Ace) => 11
      | (_,_) => 10

(* question 2c *)
fun remove_card (cards,card,e) =
    case cards of
	[] => raise e
      | c::cs => case c = card of
		     true => cs
		   | false => c::remove_card(cs,card,e)

(* question 2d *)
fun all_same_color cards = 
    case cards of
	[] => true
      | _::[] => true
      | c1::(c2::cs) =>  card_color(c1) = card_color(c2) 
			 andalso all_same_color(c2::cs)
    
(* question 2e *)
fun sum_cards cards = 
    let
	fun sum_cards_helper(cs,sum) = 
	    case cs of
		[] => sum
	     | x::xs => sum_cards_helper(xs,sum+card_value(x))
    in
	sum_cards_helper(cards,0)
    end

(* question 2f *)
fun score (cards,goal) = 
    let
	val sum = sum_cards(cards)
	val p_score = case sum - goal > 0 of
			  true => 3*(sum - goal)
			| false => (goal - sum)
    in
	case all_same_color(cards) of
	    true => p_score div 2
	  | false => p_score
    end

(* question 2g *)
fun officiate (cards,moves,goal) =
    let
	fun officiate_helper(cards,cards_on_hand,moves) =
	    case moves of
		[] => score(cards_on_hand,goal)
	      | (Discard c)::ms =>  officiate_helper(cards,remove_card(cards_on_hand,c,IllegalMove),ms)
	      | Draw::ms => case cards of
				[] =>  score(cards_on_hand,goal)
			      | c::cs => case sum_cards(c::cards_on_hand)>goal of 
					     true =>  score(c::cards_on_hand,goal)
					   | false => officiate_helper(cs,c::cards_on_hand,ms)
    in
	officiate_helper(cards,[],moves)
    end
    
(* question 3a *)
fun card_value1 card =
    case card of
	(_,Num x) => x
      | (_,Ace) => 1
      | (_,_) => 10

fun sum_cards1 cards = 
    let
	fun sum_cards_helper(cs,sum) = 
	    case cs of
		[] => sum
	     | x::xs => sum_cards_helper(xs,sum+card_value1(x))
    in
	sum_cards_helper(cards,0)
    end

fun score1 (cards,goal) = 
    let
	val sum = sum_cards1(cards)
	val p_score = case sum - goal > 0 of
			  true => 3*(sum - goal)
			| false => (goal - sum)
    in
	case all_same_color(cards) of
	    true => p_score div 2
	  | false => p_score
    end

fun score_challenge (cards,goal) = 
   let
       val score = score(cards,goal)
       val score1 = score1(cards,goal)
   in
       case score > score1 of
	   true => score1
	 | false => score
   end

fun officiate_challenge (cards,moves,goal) =
    let
	fun officiate_helper(cards,cards_on_hand,moves) =
	    case moves of
		[] => score_challenge(cards_on_hand,goal)
	      | (Discard c)::ms =>  officiate_helper(cards,remove_card(cards_on_hand,c,IllegalMove),ms)
	      | Draw::ms => case cards of
				[] =>  score_challenge(cards_on_hand,goal)
			      | c::cs => case sum_cards1(c::cards_on_hand)>goal of 
					     true =>  score_challenge(c::cards_on_hand,goal)
					   | false => officiate_helper(cs,c::cards_on_hand,ms)
    in
	officiate_helper(cards,[],moves)
    end

(* question 3b *)

fun careful_player (cards,goal) =
    let 
	fun discard_one_card(next_card,cards_on_hand,moves) =
	    let
		fun discard_one_card_helper(card_list) =
		    case card_list of
			[] => moves
		      | x::xs => case score_challenge(next_card::remove_card(cards_on_hand,x,IllegalMove),goal) = 0 of
				     true => moves@[(Discard x),Draw]
				   | fasle =>  discard_one_card_helper(xs)
	    in
		case score_challenge(cards_on_hand,goal) = 0 of
		    true => moves
		  | false => discard_one_card_helper(cards_on_hand) 
	    end		     
	fun careful_palyer_helper(cards,cards_on_hand,moves) =
	    case cards of
		[] => moves
	      | c::cs => case (goal - sum_cards1(cards_on_hand) > 10) of
			     true => careful_palyer_helper(cs,c::cards_on_hand,moves@[Draw])
			   | false =>  case cards_on_hand of
					   x::xs => discard_one_card(c,x::xs,moves)
					  |[] => case card_value1(c) > goal of
						     true => moves
						   | false => careful_palyer_helper(cs,c::cards_on_hand,moves@[Draw])
    in
	careful_palyer_helper(cards,[],[])
    end

