(* Dan Grossman, CSE341 Winter 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
let same_string(s1 : string)(s2 : string) = 
    s1 = s2

(* put your solutions for problem 1 here *)

(* Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines. *)

// Is tail recursive
let found s ss = 
  let rec found_impl sl = 
    match sl with
    | [] -> false
    | h::t -> 
      if h = s 
      then true 
      else found_impl t
  in found_impl ss

let test_recursion_found = 
  found "X" ["A";"B";"C";"D";"E";"F";"X"]

  // Not yet tail recursive
let remove_when_found s ss = 
  let rec remove_when_found_impl sl = 
    match sl with
    | [] -> []
    | h::t -> if h = s then t else h :: remove_when_found_impl t 
  in remove_when_found_impl ss
  
let test_recursion_remove_when_found = 
  remove_when_found "X" ["A";"B";"X";"D";"E";"F";"X"]

// Not yet single-pass
let all_except_option_twopass s ss =
  let found_in_list = found s ss in
    match found_in_list with
    | false -> None 
    | true -> Some (remove_when_found s ss)

let ``Not in list returns none`` = 
  all_except_option_twopass "Blah" ["A";"B"]
  
let ``In list returns rest of list`` = 
  all_except_option_twopass "A" ["A";"B";"C"]

(* Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
  substitutions) and a string s and returns a string list. The result has all the strings that are in
  some list in substitutions that also has s, but s itself should not be in the result. Example:

  get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")

  (* answer: ["Fredrick","Freddie","F"] *)

  Assume each list in substitutions has no repeats. The result will have repeats if s and another string are
  both in more than one list in substitutions. Example:

  get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")

  (* answer: ["Jeffrey","Geoff","Jeffrey"] *)

  Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines.
*)

let get_substitutions1 substitutions s = 
  let rec get_subsitutions_impl subs = 
    match subs with
    | [] -> []
    | h::t -> let matches = all_except_option_twopass s h in
              match matches with
              | None -> get_subsitutions_impl t
              | Some x -> x @ get_subsitutions_impl t
  in get_subsitutions_impl substitutions

(* answer: ["Fredrick","Freddie","F"] *)
let ``Find alternatives to Fred`` = 
  get_substitutions1 [["Fred";"Fredrick"];["Elizabeth";"Betty"];["Freddie";"Fred";"F"]] "Fred"

// Tail recursive
let get_substitutions2 substitutions s = 
  let rec get_subsitutions_impl foundSoFar subs = 
    match subs with
    | [] -> foundSoFar
    | h::t -> let matches = all_except_option_twopass s h in
                let soFar = 
                  match matches with
                  | None -> foundSoFar 
                  | Some x -> foundSoFar @ x in
                  get_subsitutions_impl soFar t
  in get_subsitutions_impl [] substitutions
  
let ``Find alternatives to Fred with tail recursion`` = 
  get_substitutions2 [["Fred";"Fredrick"];["Elizabeth";"Betty"];["Freddie";"Fred";"F"]] "Fred"

type FullName = {first:string; middle:string; last:string}

// not tail recursive 
let similar_names substitutions (fullName : FullName) = 
  let subs = get_substitutions2 substitutions fullName.first in
    let rec iterate_over_subs ss = 
      match ss with
      | [] -> []
      | h::t -> {fullName with first = h} :: iterate_over_subs t
    in fullName :: iterate_over_subs subs

let ``Iterates over Fred alternatives`` 
  = similar_names [["Fred";"Fredrick"];["Elizabeth";"Betty"];["Freddie";"Fred";"F"]] {first="Fred"; middle="W"; last="Smith"}

(* answer: [{first="Fred", last="Smith", middle="W"},
{first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"},
{first="F", last="Smith", middle="W"}] *)

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
type suit = Clubs | Diamonds | Hearts | Spades
type rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

type color = Red | Black
type move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

let card_color (card: card) =
 match card with
 | (Clubs, _) | (Spades, _) ->  color.Black
 | _ -> color.Red

let ``Club is black`` =
  card_color (Clubs, Jack)

let card_value (card: card) = 
 match card with
 | (_, Ace) -> 11
 | (_, Jack) | (_, Queen) | (_, King) -> 10
 | (_, Num num) -> num

let ``Jack is ten`` =
  card_value (Clubs, Jack)

let remove_card cards card exc = 
  let found_in_list = found card cards in
    match found_in_list with
    | false -> raise exc 
    | true -> remove_when_found card cards

let ``Removes found card`` = 
  let cards = [(Clubs,Jack);(Spades,Num(8))] 
  let card = (Spades,Num(8))
  let exc = IllegalMove
  in remove_card cards card exc

let ``Removes only first found card`` = 
  let cards = [(Clubs,Jack);(Spades,Num(8));(Spades,Num(8))] 
  let card = (Spades,Num(8))
  let exc = IllegalMove
  in remove_card cards card exc

let all_same_color cards =
  let first = 
    match cards with
    | [] -> Black
    | h :: t -> card_color h
  in 
    let rec all current remainder = 
      match remainder with
      | [] -> current
      | h::t -> if card_color h = first then all current t else false
    in all true cards

let ``Are all same colour`` = 
  all_same_color [(Clubs,Jack);(Spades,Num(8))] 

let ``Are not all same colour`` = 
  all_same_color [(Hearts,Jack);(Spades,Num(8))] 

let sum_cards cards = 
  let rec sum current remainder = 
    match remainder with
    | [] -> current
    | h::t -> sum (current + card_value h) t
  in sum 0 cards

let ``Sums to 18`` = 
  sum_cards [(Clubs,Jack);(Spades,Num(8))] 

let preliminary_score goal sum = 
  if sum > goal then (sum - goal) * 3 else goal - sum

let get_score goal sum cards = 
  let preliminary = preliminary_score goal sum in 
    if all_same_color cards then preliminary / 2 else preliminary

let officiate cardsInput movesInput goalInput =
  let rec process_moves cards moves goal score held = 
   match moves with
   | [] -> get_score goal score held
   | move :: remainingMoves -> 
     match move with
     | Draw -> 
       match cards with 
       | [] -> get_score goal score held 
       | drawnCard :: remainingCards -> 
         let newScore = (card_value drawnCard + score) in 
         match (newScore > goal) with 
         | true -> get_score goal newScore held
         | false -> process_moves remainingCards remainingMoves goal newScore (drawnCard :: held)
     | Discard card -> process_moves cards remainingMoves goal (score - card_value card) (remove_card held card IllegalMove)
  in process_moves cardsInput movesInput goalInput 0 []


(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

let try_officiate cards moves goal = 
  try 
    officiate cards moves goal
  with 
    | IllegalMove -> -1

let provided_test1 = (* correct behavior: raise IllegalMove *)
    let cards = [(Clubs,Jack);(Spades,Num(8))] 
    let moves = [Draw;Discard(Hearts,Jack)]
    in try_officiate cards moves 42

let provided_test2 = (* correct behavior: return 3 *)
    let cards = [(Clubs,Ace);(Spades,Ace);(Clubs,Ace);(Spades,Ace)]
    let moves = [Draw;Draw;Draw;Draw;Draw]
    in officiate cards moves 42
