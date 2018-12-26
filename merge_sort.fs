open System;

let unsorted_list = [9;2;5;1;4;8;3;7]

let rec print_list lst =
  match lst with
  | [a] -> Console.WriteLine(a.ToString())
  | a::tail -> Console.Write(a.ToString() + ", "); print_list tail

let rec merge_sort lst = 
  let rec halve lst =
    let rec zip lst =
      match lst with
      | [] -> []
      | [a] -> [(a, Int32.MinValue)] //Int32.MinValue is used because I can't do (a,)
      | a::b::tail -> (a,b) :: zip(tail)

    let rec first x =
      match x with
      | [] -> []
      | (Int32.MinValue, _)::tail -> first tail
      | (a, _)::tail -> a :: first tail

    let rec second x =
      match x with
      | [] -> []
      | (_, Int32.MinValue):: tail -> second tail
      | (_, b)::tail -> b :: second tail

    //convert list into (a,b) tuples: e.g. [1;2;3;4] => [(1,2), (3,4)]
    let r = zip lst
    //grab all of the first values from the tuples and returns a list
    //e.g. [(1,2),(3,4)] => [1;3]
    let f = first r
    //grab all of the first values from the tuples and returns a list
    //e.g. [(1,2),(3,4)] => [2;4]
    let s = second r

    f, s //return the two lists

  let rec merge left right =
    //match both lists
    match (left, right) with
    //matching both lists, always pick the lowest number
    | a::x, b::y -> if a < b then a :: merge x (b::y) else b :: merge (a::x) y
    | a, [] -> a
    | [], b -> b
    | [], [] -> []

  let caseTwo arr =
    //split the list into half
    let (left, right) = halve arr
    //sort each side
    let left = merge_sort left
    let right = merge_sort right

    //merge the sorted sides
    merge left right

  match lst with
  | [a] -> [a]
  | _ -> caseTwo lst

let sorted_lst = merge_sort unsorted_list
for x in sorted_lst do
  Console.WriteLine(x)