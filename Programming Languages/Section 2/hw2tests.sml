(* Tests for Homework 2 *)
(* All the tests should evaluate to true. At the end the REPL should say: val test4all = true : bool *)
use "hw2provided.sml";

(* Test for all_except_option *)
val test1a1 = all_except_option("a",[]) = NONE
val test1a2 = all_except_option("a",["a"]) = SOME []
val test1a3 = all_except_option("b",["a","b","c"]) = SOME (["a","c"])

(* Test for get_substitutions1 *)
val test1b1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test1b2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test1b3 = get_substitutions1([],"a") = []

(* Test for get_substitutions2 *)
val test1c1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test1c2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test1c3 = get_substitutions2([],"a") = []

val test1d1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"},{first="Fredrick", last="Smith", middle="W"},{first="Freddie", last="Smith", middle="W"},{first="F", last="Smith", middle="W"}]

(* Test for question 1 *)
val test1 = test1a1 andalso test1a2 andalso test1a3 andalso test1b1 andalso test1b2 andalso test1b3 andalso test1c1 andalso test1c2 andalso test1c3 andalso test1d1

(* Test for card_color *)
val test2a1 = card_color(Clubs,Jack) = Black
val test2a2 = card_color(Diamonds,Num 6) = Red
val test2a3 = card_color(Hearts,Num 10) = Red
val test2a4 = card_color(Spades,Num 9) = Black

(* Test for card_value *)
val test2b1 = card_value(Clubs,Num 4) = 4
val test2b2 = card_value(Hearts,Ace) = 11
val test2b3 = card_value(Spades,King) = 10

(* Test Data *)
exception remove_card_exception
val cards = [(Clubs,Jack),(Diamonds,Num 6),(Hearts,Num 10),(Spades,Num 9),(Clubs,Num 4),(Hearts,Ace),(Diamonds,Num 6),(Clubs,Queen),(Hearts,Num 8),(Spades,Num 2)] (* sum=76 *)
val cards1 =  [(Clubs,Jack),(Hearts,Num 10),(Spades,Num 9),(Clubs,Num 4),(Hearts,Ace),(Diamonds,Num 6),(Clubs,Queen),(Hearts,Num 8),(Spades,Num 2)]
val moves1 = [Draw,Draw,Draw,Draw,(Discard (Hearts,Num 10)),Draw] (* sum=29 *)
val moves2 = [Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw] 

(* Test for remove_card *)
val test2c1 = remove_card(cards,(Diamonds,Num 6),remove_card_exception) = cards1
val test2c2 = (remove_card(cards,(Diamonds,Queen),remove_card_exception) handle remove_card_exception => []) = []

(* Test for all_same_color *)
val test2d1 = all_same_color([]) = true
val test2d2 = all_same_color(cards1) = false
val test2d3 = all_same_color([(Spades,Num 2)]) = true
val test2d4 = all_same_color([(Spades,Num 3),(Hearts,Num 6)]) = false
val test2d5 = all_same_color([(Diamonds,Num 3),(Hearts,Num 6)]) = true

(* Test for sum_cards *)
val test2e1 = sum_cards(cards) = 76
val test2e2 = sum_cards([]) = 0

(* Test for score *)
val test2f1 = score(cards,76) = 0
val test2f2 = score([],6) = 3
val test2f3 = score(cards,70) = 18
val test2f4 = score(cards,80) = 4
val test2f5 = score([(Diamonds,Num 3),(Hearts,Num 6)],6) = 4

(* Test for officiate *)
val test2g1 = officiate(cards,moves1,36) = 7
val test2g2 = officiate(cards,moves2,76) = 0
val test2g3 = officiate(cards,moves2,33) = 6
val test2g4 = (officiate(cards,[(Discard (Hearts,Ace))],12) handle IllegalMove => ~1) = ~1

(* Test for question 2 *)
val test2 = test2a1 andalso test2a2 andalso test2a3 andalso test2a4 andalso test2b1 andalso test2b2 andalso test2b3 andalso test2c1 andalso test2c2 andalso test2d1 andalso test2d2 andalso test2d3 andalso test2d4 andalso test2d5 andalso test2e1 andalso test2e2 andalso test2f1 andalso test2f2 andalso test2f3 andalso test2f4 andalso test2f5 andalso test2g1 andalso test2g2 andalso test2g3 andalso test2g4

(* Test for officiate_challenge *)
val test3a1 = officiate_challenge(cards,moves2,66) = 0

(* Test for careful_player *)
val test3b1 = careful_player(cards,10) = [Draw]
val test3b2 = careful_player(cards,27) = [Draw,Draw,Draw]
val test3b3 = careful_player(cards,52) = [Draw,Draw,Draw,Draw,Draw,Draw,Draw,Discard (Clubs,Num 4),Draw]

(* Test for question 3 *)
val test3 = test3a1 andalso test3b1 andalso test3b2 andalso test3b3

(* Test for all *)
val test4all = test1 andalso test2 andalso test3
