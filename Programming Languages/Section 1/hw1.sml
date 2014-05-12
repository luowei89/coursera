(* Homework1 *)

(* (int * int * int) * (int * int * int) -> bool *)
(* given two dates date1 and date2 *) 
(* returns ture iff date1 is older than date2 *)
fun is_older (date1 : int * int * int, date2 : int * int * int) = 
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date1) > (#1 date2)
    then false
    else if (#2 date1) < (#2 date2)
    then true
    else if (#2 date1) > (#2 date2)
    then false
    else if (#3 date1) < (#3 date2)
    then true
    else false
(* (int * int * int) list * int -> int *)
(* takes a list of dates and a month *)
(* returns how many dates in the list are in the given month *)
fun number_in_month (dates : (int * int * int) list , month : int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month
    then 1 + number_in_month((tl dates), month)
    else number_in_month((tl dates), month)
(* (int * int * int) list * int list -> int *)
(* takes a list of dates and a list of months  *)
(* returns the number of dates in the list of dates that are in any of the months in the list of months *)
fun number_in_months (dates : (int * int * int) list , months : int list) =
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))
(* (int * int * int) list * int -> (int * int * int) list *)
(* takes a list of dates and a month *)
(* returns a list holding the dates from the argument list of dates that are in the month *)
fun dates_in_month (dates : (int * int * int) list , month : int) = 
    if null dates
    then []
    else if (#2 (hd dates)) = month
    then (hd dates)::dates_in_month((tl dates),month)
    else dates_in_month((tl dates), month)
(* (int * int * int) list * int list -> (int * int * int) list *)
(* takes a list of dates and a list of months *)
(* returns a list holding the dates from the argument list of dates that are in any of the months in the list of months *)
fun dates_in_months (dates : (int * int * int) list , months : int list) =
    if null months
    then []
    else dates_in_month(dates,(hd months))@dates_in_months(dates,(tl months))
(* string list * int -> string *)
(* takes a list of strings and an int n *)
(* returns the nth element of the list where the head of the list is 1st *)
fun get_nth (strings : string list , n : int) =
    if null strings
    then ""
    else if n < 1
    then ""
    else if n = 1
    then hd strings
    else get_nth((tl strings),n-1)
(* int * int * int -> string *)
(* takes a date *)
(*  returns a string of the form January 20, 2013 (for example) *)
fun date_to_string (date : int * int * int) = 
    let
	val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
    in
	get_nth(months,(#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end
(* int * int list -> int *)
(* takes an int called sum, and an int list *)
(* assume that sum is positive, and list contain all positive numbers, sum of all elements in list greater than sum*)
(* returns an int n which indicates the sum of first n elements in the list < sum and sum of first n+1 elements >= sum *)
fun number_before_reaching_sum (sum : int, n_list: int list) = 
    if null n_list
    then 0
    else if sum <= (hd n_list)
    then number_before_reaching_sum(sum - (hd n_list),(tl n_list))
    else 1 + number_before_reaching_sum(sum- (hd n_list),(tl n_list))
(* int -> int *)
(* takes a day of year *)
(* returns what month that day s in *)
fun what_month (day : int) =
    let
	val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1 + number_before_reaching_sum(day, days)
    end
(* int * int -> int list *)
(*  takes two days of the year day1 and day2  *)
(* returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2 *)
fun month_range (day1 : int, day2 : int) =
    let
	fun count (from : int, to : int) =
	    if from > to
	    then []
	    else if from = to
	    then to::[]
	    else from::count(from+1,to)
	val days = count(day1,day2)
	fun month_ranges (days : int list) =
	    if null days
	    then []
	    else what_month(hd days)::month_ranges(tl days)
    in
	month_ranges(days)
    end
(* (int * int * int) list -> (int * int * int) option *)
(* takes a list of dates *)
(* reuturns an (int*int*int) option: NONE if the list has no dates and SOME d if the date d is the oldest date in the list *)
fun oldest (dates : (int * int * int) list) = 
    let
	fun older (date1 : (int * int * int), date2 : (int * int * int)) = 
	    if is_older(date1, date2)
	    then date1
	    else date2
    in
	if null dates
	then NONE
	else if null (tl dates)
	then SOME (hd dates)
	else SOME (older((hd dates),valOf(oldest(tl dates))))
    end
(* int list -> int list *)
(* takes a list of int *)
(* returns a list without duplicates in the given list *)
fun remove_duplicates (list : int list) = 
    if null list
    then []
    else
	let 
	    fun in_list (n : int, list : int list) =
		if null list
		then false
		else if (hd list) = n
		then true
		else false orelse in_list(n,(tl list))
	    val list_tl = remove_duplicates(tl list)
	in
	    if in_list((hd list),list_tl)
	    then list_tl
	    else (hd list)::list_tl
	end
(* (int * int * int) list * int list -> int *)
(* takes a list of dates and a list of months  *)
(* returns the number of dates in the list of dates that are in any of the months in the list of months *)
fun number_in_months_challenge (dates : (int * int * int) list , months : int list) =
    let
	val months_no_duplicates = remove_duplicates(months)
    in
	number_in_months(dates,months_no_duplicates)
    end
(* (int * int * int) list * int list -> (int * int * int) list *)
(* takes a list of dates and a list of months *)
(* returns a list holding the dates from the argument list of dates that are in any of the months in the list of months *)
fun dates_in_months_challenge (dates : (int * int * int) list , months : int list) =
    let
	val months_no_duplicates = remove_duplicates(months)
    in
	dates_in_months(dates,months_no_duplicates)
    end
(* (int * int * int) -> bool*)
(* takes a date *)
(* returns whether the given date is reasonable*)
fun reasonable_date(date : int * int * int) = 
    let
	val days = [31,28,31,30,31,30,31,31,30,31,30,31]
	val leap_days = [31,29,31,30,31,30,31,31,30,31,30,31]
	fun leap_year(year : int) =
	    year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
	fun get_nth_num (list : int list , n : int) =
	    if null list
	    then 0
	    else if n < 1
	    then 0
	    else if n = 1
	    then hd list
	    else get_nth_num((tl list),n-1)
    in
	if #1 date <= 0
	then false
	else if #2 date <=0 orelse #2 date >12
	then false
	else if leap_year(#1 date)
	then #3 date > 0 andalso #3 date <= get_nth_num(leap_days,(#2 date))
	else #3 date > 0 andalso #3 date <= get_nth_num(days,(#2 date))
    end
