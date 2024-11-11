let SHOW x = printf "%A\n" x
type Tree =
    | Empty
    | Node of int * Tree * Tree

//#### Binary Tree as DU

let tree =
    Node (8,
         Node(3, Node(1, Empty, Empty), Node(6, Node(4, Empty, Empty), Node(7, Empty, Empty))),
         Node(10, Empty, Node(14, Node(13, Empty, Empty), Empty)))

//### Exercise 1.4
//##Insert element into Binary Search Tree

//#### --------------- Your code goes below --------------- *)
let rec insertBST (value: int) (tree: Tree): Tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(node, left, right) when value <= node -> Node(node, insertBST value left, right)
    | Node(node, left, right) -> Node(node, left, insertBST value right)

let ``exercise 1.4 raw`` = insertBST 5 tree 
//** #### Value of ``exercise 1.4`` *)
SHOW ``exercise 1.4 raw``

//### Exercise 3.1
//##Implement `parseScore`.

//#### --------------- Your code goes below --------------- *)

let (|Digit|_|) char =
    let zero = System.Convert.ToInt32 '0'
    if System.Char.IsDigit char then Some(System.Convert.ToInt32 char - zero) else None
    
let rec parseScore (chars: char list): int option list =
    match chars with
    | [] -> []
    | 'X' :: tail -> Some(10) :: parseScore(tail)
    | Digit d :: '/' :: tail -> Some(d) :: Some(10-d) :: parseScore(tail)
    | '-' :: '/' :: tail -> Some(0) :: Some(10) :: parseScore(tail)
    | '-' :: tail -> Some(0) :: parseScore(tail)
    | Digit d :: tail -> Some(d) :: parseScore(tail)
    | _ :: tail -> None :: parseScore(tail)

let ``exercise 3.1`` =
    parseScore [ 'X'
                 '4'
                 '/'
                 '2'
                 '-'
                 'N' ]
//** #### Value of ``exercise 3.1`` *)
SHOW ``exercise 3.1``

//### Exercise 3.2
//##Implement `countScore`

//#### --------------- Your code goes below --------------- *)
let rec countScore (scores: int list): int =
    match scores with
    | 10 :: (10 :: 10 :: _ as tail) -> 30 + countScore (tail) // turkey
    | 10 :: (10 :: i :: j :: _ as tail) -> 20 + i + j + countScore(tail) // double
    | 10 :: (_ :: [ _ ] as tail) -> 10 +  countScore(tail) // skip strike bonus in final frame
    | 10 :: (i :: j :: _ as tail) -> 10 + i + j + countScore(tail) // strike
    | 10 :: [ 10 ]  -> 0 // ignore strike in final frame
    | i :: j :: (k :: _ as tail) when i + j = 10 -> 10 + k + countScore(tail) // spare
    | i :: j :: tail -> i + j + countScore(tail)
    | [ _ ] | [] -> 0

let ``exercise 3.2`` =
    [ [ 10
        10
        10
        10
        10
        10
        10
        10
        10
        10
        10
        10 ]
      [ 9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0 ]
      [ 5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5 ]
      [ 10
        9
        1
        5
        5
        7
        2
        10
        10
        10
        9
        0
        8
        2
        9
        1
        10 ] ]
    |> List.map countScore
//** #### Value of ``exercise 3.2`` *)
SHOW ``exercise 3.2``

//### sequenceOpts function *)
let sequenceOpts (optionals: 'a option list): 'a list option =
    let rec sequence' acc optionals =
        match optionals, acc with
        | [], _ -> Option.map List.rev acc
        | Some h :: t, Some acc -> sequence' (Some(h :: acc)) t
        | _ -> None

    sequence' (Some []) optionals


//### Homework 1
//##Implement `bowlingScore`.

//###Hint: Use `sequenceOpts` to convert from list of options to option of list
let bowlingScore (score: string): int =
    match Seq.toList score |> parseScore |> sequenceOpts with
    | Some list -> countScore list
    | None -> 0

let ``homework 1`` =
    [ "XXXXXXXXXXXX"
      "9-9-9-9-9-9-9-9-9-9-"
      "9--/9-9-9-9-9-9-9-9-"
      "X-/9-9-9-9-9-9-9-9-"
      "9-X9-9-X--9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-XXX"
      "5/5/5/5/5/5/5/5/5/5/5"
      "5/5/5/5/5/5/5/5/5/5/X"
      "X9/5/72XXX9-8/9/X" ]
    |> List.map bowlingScore

//** #### Value of ``homework 1`` *)
SHOW ``homework 1``

//EXPECTED RESULTS   
//[Some 300; Some 90; Some 100; Some 111; Some 92;
//   Some 90; Some 111; Some 150; Some 155; Some 187]


//### Homework 2
//###Write new, **tail-recursive** versions of `parseScore` and `countScore`.
//###Implement `bowlingScoreTail` to use those 2 new functions

let rec parseScoreTail (chars: char list) (acc: int option list): int option list =
    match chars with
    | [] -> List.rev acc
    | 'X' :: tail -> parseScoreTail tail (Some(10) :: acc)
    | Digit d :: '/' :: tail -> parseScoreTail tail (Some(10-d) :: Some(d) :: acc)
    | '-' :: '/' :: tail ->  parseScoreTail tail (Some(10) :: Some(0) :: acc)
    | '-' :: tail -> parseScoreTail tail (Some(0) :: acc)
    | Digit d :: tail -> parseScoreTail tail (Some(d) :: acc)
    | _ :: tail -> parseScoreTail tail (None :: acc)

let rec countScoreTail (scores: int list) (acc: int): int =
    match scores with
    | 10 :: (10 :: 10 :: _ as tail) -> countScoreTail(tail)(30 + acc) // turkey
    | 10 :: (10 :: i :: j :: _ as tail) -> countScoreTail(tail)(20 + i + j + acc) // double
    | 10 :: (_ :: [ _ ] as tail) -> countScoreTail(tail)(10 + acc) // skip strike bonus in final frame
    | 10 :: (i :: j :: _ as tail) -> countScoreTail(tail)(10 + i + j + acc) // strike
    | 10 :: [ 10 ]  -> acc // ignore strike in final frame
    | i :: j :: (k :: _ as tail) when i + j = 10 -> countScoreTail(tail)(10 + k + acc) // spare
    | i :: j :: tail -> countScoreTail(tail)(i + j + acc)
    | [ _ ] | [] -> acc

let bowlingScoreTail (score: string): int option =
    match parseScoreTail (Seq.toList score) [] |> sequenceOpts with
    | Some list -> Some(countScoreTail list 0)
    | None -> None

let ``homework 2`` =     
    [ "XXXXXXXXXXXX"
      "9-9-9-9-9-9-9-9-9-9-"
      "9--/9-9-9-9-9-9-9-9-"
      "X-/9-9-9-9-9-9-9-9-"
      "9-X9-9-X--9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-XXX"
      "5/5/5/5/5/5/5/5/5/5/5"
      "5/5/5/5/5/5/5/5/5/5/X"
      "X9/5/72XXX9-8/9/X" ]
    |> List.map bowlingScoreTail 
//** #### Value of ``homework 2`` *)
SHOW ``homework 2``

//EXPECTED RESULTS   
//[Some 300; Some 90; Some 100; Some 111; Some 92;
//   Some 90; Some 111; Some 150; Some 155; Some 187]

//////////////////////////////////////////////////////////////
/// Indeks: 184347
/// Imię: Jakub
/// Nazwisko: Dajczak
/// 
/// Podsumowanie zalizowanych zadan:
/// Exercise 1.4 zrobione
/// Exercise 3.1 zrobione
/// Exercise 3.2 zrobione
/// Homework 1 zrobione
/// Homework 2 zrobione