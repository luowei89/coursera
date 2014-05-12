(* Tests for homework1 *)
(* All the tests should evaluate to true. At the end the REPL should say: val test4all = true : bool *)
use "hw1.sml";

(* Tests for is_older *)
val test011 = is_older((2000,10,13),(2001,1,29)) = true
val test012 = is_older((2001,1,29),(2000,10,13)) = false
val test013 = is_older((2000,10,13),(2000,11,18)) = true
val test014 = is_older((2000,11,18),(2000,10,13)) = false
val test015 = is_older((2001,1,29),(2001,1,30)) = true
val test016 = is_older((2001,1,30),(2001,1,29)) = false
val test017 = is_older((2001,1,29),(2001,1,29)) = false
val test01 = test011 andalso test012 andalso test013 andalso test014 andalso test015 andalso test016 andalso test017
(* Tests for number_in_month *)
val test021 = number_in_month([(2000,1,1),(2000,1,12),(2000,2,3)],1) = 2
val test022 = number_in_month([(2000,1,1),(2000,1,12),(2000,2,3)],2) = 1
val test023 = number_in_month([(2000,1,1),(2000,1,12),(2000,2,3)],3) = 0
val test024 = number_in_month([],3) = 0
val test02 = test021 andalso test022 andalso test023 andalso test024
(* Tests for number_in_months *)
val test031 = number_in_months([(2000,1,21),(2000,1,31),(2000,4,5),(2000,2,12)], [1,2]) = 3
val test032 = number_in_months([(2000,12,31),(2000,1,21)], []) = 0
val test033 = number_in_months([],[1]) = 0
val test03 =  test031 andalso test032 andalso test033
(* Tests for dates_in_month *)
val test041 = dates_in_month([(2000,1,1),(2000,1,12),(2000,2,5)],1) = [(2000,1,1),(2000,1,12)]
val test042 = dates_in_month([],2) = []
val test04 = test041 andalso test042
(* Tests for dates_in_months *)
val test051 = dates_in_months([(2000,1,23),(2000,3,4),(2000,2,21)],[1,2]) = [(2000,1,23),(2000,2,21)]
val test052 = dates_in_months([(2000,1,10),(2000,2,20)],[]) = []
val test053 = dates_in_months([],[1,2,3]) = []
val test05 = test051 andalso test052 andalso test053
(* Tests for get_nth *)
val test061 = get_nth(["a","b","c"],2) = "b"
val test062 = get_nth([],1) = ""
val test063 = get_nth(["a"],2) = ""
val test064 = get_nth(["a","b"],0) = ""
val test06 = test061 andalso test062 andalso test063 andalso test064
(* Tests for date_to_string *)
val test07 = date_to_string((2013,1,20)) = "January 20, 2013"
(* Tests for number_before_reaching_sum *)
val test081 = number_before_reaching_sum(12,[10,1,3,4,5]) = 2
val test082 = number_before_reaching_sum(10,[1,2,3,4,5]) = 3
val test08 = test081 andalso test082
(* Tests for what_month *)
val test091 = what_month(334) = 11
val test092 = what_month(335) = 12
val test093 = what_month(365) = 12
val test09 = test091 andalso test092 andalso test093
(* Tests for month_range *)
val test101 = month_range(30,35) = [1,1,2,2,2,2]
val test102 = month_range(335,335) = [12]
val test103 = month_range(320,12) = []
val test10 = test101 andalso test102 andalso test103
(* Tests for oldest *)
val test111 = oldest([(2001,1,12),(2001,1,24),(2002,2,9)]) = SOME (2001,1,12)
val test112 = oldest([]) = NONE
val test113 = oldest([(2013,8,23)]) = SOME (2013,8,23)
val test11 = test111 andalso test112 andalso test113
(* Test for number_in_months_challenge and dates_in_months_challenge *)
val test121 =           number_in_months([(2000,1,21),(2000,1,31),(2000,4,5),(2000,2,12)], [1,2,2]) = 4
val test122 = number_in_months_challenge([(2000,1,21),(2000,1,31),(2000,4,5),(2000,2,12)], [1,2,2]) = 3
val test123 = number_in_months_challenge([(2000,12,31),(2000,1,21)], []) = 0
val test124 = number_in_months_challenge([],[1]) = 0
val test125 =           dates_in_months([(2000,1,23),(2000,3,4),(2000,2,21)],[1,2,2]) = [(2000,1,23),(2000,2,21),(2000,2,21)]
val test126 = dates_in_months_challenge([(2000,1,23),(2000,3,4),(2000,2,21)],[1,2,2]) = [(2000,1,23),(2000,2,21)]
val test127 = dates_in_months_challenge([(2000,1,10),(2000,2,20)],[]) = []
val test128 = dates_in_months_challenge([],[1,2,3]) = []
val test12 = test121 andalso test122 andalso test123 andalso test124 andalso test125 andalso test126 andalso test127 andalso test128
(* Tests for reasonable_date *)
val test131 =  reasonable_date(0,1,12) = false
val test132 =  reasonable_date(1900,2,29) = false
val test133 =  reasonable_date(2000,2,29) = true
val test134 =  reasonable_date(2000,0,10) = false
val test135 =  reasonable_date(2000,100,1) = false
val test136 =  reasonable_date(2000,1,32) = false
val test137 =  reasonable_date(2000,1,0) = false
val test138 =  reasonable_date(2013,10,10) = true
val test13 = test131 andalso test132 andalso test133 andalso test134 andalso test135 andalso test136 andalso test137 andalso test138
(* Test for all *)
val test4all = test01 andalso test02 andalso test03 andalso test04 andalso test05 andalso test06 andalso test07 andalso test08 andalso test09 andalso test10 andalso test11 andalso test12 andalso test13
