use "hw1main.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test31 = number_in_months([(2023,2,21),(2023,12,3),(2023,3,11)], []) = 0
val test32 = number_in_months([(2023,2,21),(2023,12,3),(2023,3,11)], [12]) = 1
val test33 = number_in_months([], []) = 0

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test41 = dates_in_month ([], 3) = []
val test42 = dates_in_month ([(2023,2,21),(2023,12,3),(2023,2,11)], 2) = [(2023,2,21),(2023,2,11)]