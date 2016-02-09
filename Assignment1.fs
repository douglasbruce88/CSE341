type Date = { Year : int; Month : int; Day : int }

(* Question 1 *)
let is_older (firstDate : Date) (secondDate : Date) =
  let compareInts x y = if x < y
                        then 1
                        else if y > x
                        then -1
                        else 0
  in
      if compareInts firstDate.Year secondDate.Year = 1 then true
      else if compareInts firstDate.Year secondDate.Year = -1 then false
      else if compareInts firstDate.Month secondDate.Month = 1 then true
      else if compareInts firstDate.Month secondDate.Month = -1 then false
      else if compareInts firstDate.Day secondDate.Day = 1 then true
      else false

// True
let ``Second year greater than first year returns true`` =
  is_older { Year = 2011; Month = 01; Day = 01} { Year = 2012; Month = 01; Day = 01}

// False
let ``First year greater than second year returns false`` =
  is_older { Year = 2012; Month = 01; Day = 01} { Year = 2011; Month = 01; Day = 01}

// True
let ``Second month greater than first month returns true`` =
  is_older { Year = 2011; Month = 01; Day = 01} { Year = 2011; Month = 02; Day = 01}

// False
let ``First month greater than second month returns false`` =
  is_older { Year = 2011; Month = 02; Day = 01} { Year = 2011; Month = 01; Day = 01}

// True
let ``Second day greater than first day returns true`` =
  is_older { Year = 2011; Month = 01; Day = 01} { Year = 2011; Month = 01; Day = 02}

// False
let ``First day greater than second day returns false`` =
  is_older { Year = 2011; Month = 01; Day = 02} { Year = 2011; Month = 01; Day = 01}

// False
let ``Same dates returns false`` =
  is_older { Year = 2011; Month = 01; Day = 01} { Year = 2011; Month = 01; Day = 01}

(* Question 2 *)
let number_in_month (dates : Date list) (month : int) =
  let rec number_in_month_impl (dateList : Date list) =
    if List.isEmpty dateList then 0
    else let headIsInMonth = if (List.head dateList).Month = month then 1 else 0
         in headIsInMonth + number_in_month_impl (List.tail dateList)
  in number_in_month_impl dates

let ``Empty list returns 0`` =
  number_in_month [] 1

let ``No dates in month returns 0`` =
  number_in_month  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 01; Day = 02}] 3

let ``Both dates in month returns 2`` =
  number_in_month  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 01; Day = 02}] 1

let ``Either dates in month returns 1`` =
  number_in_month  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 02; Day = 02}] 1

(* Question 3 *)
let rec number_in_months (dates: Date list) (months : int list) =
  if List.isEmpty months then 0
  else number_in_month dates (List.head months) + number_in_months dates (List.tail months)

let ``Empty month list returns 0`` =
  number_in_months [] []

let ``No dates in month list returns 0`` =
  number_in_months  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 01; Day = 02}] [3]

let ``Both dates in month list returns 2`` =
  number_in_months  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 02; Day = 02}] [1;2]

let ``Either dates in month list returns 1`` =
  number_in_months  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 02; Day = 02}] [1;3]

(* Question 4 *)
let dates_in_month (dates : Date list) (month : int) =
  let rec dates_in_month_impl (dateList : Date list) =
    if List.isEmpty dateList then List.empty
    else if (List.head dateList).Month = month
    // Can we consolidate these branches somehow?
    then (List.head dateList) :: dates_in_month_impl (List.tail dateList)
    else dates_in_month_impl (List.tail dateList)
  in dates_in_month_impl dates

let ``Empty list returns empty`` =
  dates_in_month [] 1

let ``No dates in month returns empty`` =
  dates_in_month  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 01; Day = 02}] 3

let ``Both dates in month returns both dates`` =
  dates_in_month  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 01; Day = 02}] 1

let ``Either dates in month returns maching date`` =
  dates_in_month  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 02; Day = 02}] 1

(* Question 5 *)
let rec dates_in_months  (dates: Date list) (months : int list) =
  if List.isEmpty months then []
  else (dates_in_month dates (List.head months)) @ (dates_in_months dates (List.tail months))

let ``Empty month list returns empty`` =
  dates_in_months [] []

let ``No dates in month list returns empty`` =
  dates_in_months  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 01; Day = 02}] [3]

let ``Both dates in month list returns both dates`` =
  dates_in_months  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 02; Day = 02}] [1;2]

let ``Either dates in month list returns maching date`` =
  dates_in_months  [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 02; Day = 02}] [1;3]

(* Question 6 *)
let get_nth (strings : string List) (n : int) =
  let rec skip_while (stringList : string List) (counter : int) =
    if n = counter
    then List.head stringList
    else skip_while (List.tail stringList) (counter + 1)
  in skip_while strings 1

let ``Asking for first element returns one`` =
  get_nth ["one";"two";"three"] 1

let ``Asking for third element returns three`` =
  get_nth ["one";"two";"three"] 3

(* Question 7 *)
#nowarn "62"
let date_to_string (date : Date)=
  let monthNames = ["January";"February";"March"]
  in get_nth monthNames date.Month ^ " " ^ date.Day.ToString() ^ ", " ^ date.Year.ToString()

let ``Formats as January 1, 2011`` =
 date_to_string { Year = 2011; Month = 01; Day = 01}

(* Question 8 *)
let number_before_reaching_sum (target : int) (numbers : int list) =
  let rec sum_while (numberList : int list) (current_sum : int) (current_index : int) =
    if List.isEmpty numberList
    then current_index
    else let new_sum = current_sum + (List.head numberList)  in
           if new_sum >= target
           then current_index
           else sum_while (List.tail numberList) new_sum current_index + 1
  in sum_while numbers 0 0

let ``Sums whole list when target is bigger (returns 4)`` =
  number_before_reaching_sum 11 [1;2;3;4]

let ``Carries on until sum is at least target (returns 2)`` =
  number_before_reaching_sum 4 [1;2;3;4]

let ``Stops if sum equals target (returns 2)`` =
  number_before_reaching_sum 6 [1;2;3;4]

(* Question 9 *)
let what_month (day : int) =
  let days_in_months = [31;28;31;30;31;30;31;31;30;31;30;31]
  in number_before_reaching_sum day days_in_months + 1

let ``31st day of the year is in 1st month`` =
  what_month 31

let ``32nd day of the year is in 2nd month`` =
  what_month 32

(* Question 10 *)
let rec month_range (day1 : int) (day2 : int) =
  if day1 > day2 then List.empty
  else what_month day1 :: month_range (day1 + 1) day2

let ``33rd to 30th is an empty range`` =
    month_range 33 30

let ``30th to 33rd days lie two in 1st month and two in 2nd`` =
  month_range 30 33

(* Question 11 *)
let oldest (dates : Date list) =
  if List.isEmpty dates
  then None
  else let rec oldest_nonempty (dateList : Date list) =
         if (List.isEmpty (List.tail dateList))
         then List.head dateList
         else let tail_oldest = oldest_nonempty (List.tail dateList)
              in if is_older (List.head dateList) tail_oldest
                 then List.head dateList
                 else tail_oldest
       in Some(oldest_nonempty dates)

let ``Empty list has no oldest date`` =
 oldest List.empty

let ``2011 01 01 is oldest`` =
  oldest [{ Year = 2011; Month = 01; Day = 01}; { Year = 2011; Month = 02; Day = 02}]
