open System;

let unsorted_list = [9;2;5;1;4;8;3;7]

let rec print_list lst =
  match lst with
  | [] -> Console.WriteLine()
  | [a] -> Console.WriteLine(a.ToString())
  | a::tail -> Console.Write(a.ToString() + ", "); print_list tail

let rec quick_sort (lst: List<int>) =
  let rec low lst p =
    match lst with
    | head::tail -> if head < p then head :: low tail p else low tail p
    | [] | [_] -> []

  let rec high lst p =
    match lst with
    | head::tail -> if head > p then head :: high tail p else high tail p
    | [] | [_] -> []

  let caseThree (arg: List<int>) =
    //pick a pivot somewhere in the middle of the list
    let pivot = arg.Item((arg.Length / 2) - 1)

    //get every number from the list that is lower than the pivot and sort it
    let left = quick_sort (low arg pivot)
    //get every number from the list that is higher than the pivot and sort it
    let right = quick_sort (high arg pivot)

    //merge the lists and pivot as: left + pivot + right
    left @ (pivot :: right)
    

  match lst with
  | [] -> [] //base case: empty list = empty list
  | [a] -> [a] //base case: list with one item can just be itself. its already 'sorted'
  | x::y -> caseThree (x::y)

let sorted_lst = quick_sort unsorted_list
for x in sorted_lst do
  Console.WriteLine(x)
