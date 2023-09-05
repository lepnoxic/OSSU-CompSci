(* Homework 1 - Programming Languages A *)


(* (int * int * int) (int * int * int) -> bool *)
(* Produce true if dateA comes before dateB else false *)

fun is_older(dateA: int * int * int, dateB: int * int * int) =
    ((#1 dateA) < (#1 dateB)) andalso ((#2 dateA) < (#2 dateB)) andalso ((#3 dateA) < (#3 dateB))


(* ((int * int * int) list) * int -> int *)
(* Produce the number of dates that are in the given month *)
fun number_in_month(dates: (int * int * int) list, month: int) =
    if null dates
        then 0
        else 
            if (#2 (hd dates) = month)
            then 1 + (number_in_month ((tl dates), month))
            else number_in_month((tl dates), month)    

(* (int * int * int) list * int list -> int *)
(* Produce number of dates which have a month the given list of months *)
(* ASSUME = list of months has no repeated number *)
fun number_in_months(dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* (int * int * int) list * int -> (int * int * int) list *)
(* Produce list of dates that are in the given month *)
fun dates_in_month(dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else 
        if (#2 (hd dates) = month) 
        then hd dates :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* (int * int * int) list * int list -> (int * int * int) list *)
(* Produce list of dates that are in the given list of months *)
fun dates_in_months(dates: (int * int * int) list, months: int list) = 
    
