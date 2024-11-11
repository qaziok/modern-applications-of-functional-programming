let SHOW x = printf "%A\n" x

//### Exercise 3.1
//##Implement `parseScore`.

//#### --------------- Your code goes below --------------- *)

let (|Digit|_|) char =
    let zero = System.Convert.ToInt32 '0'
    if System.Char.IsDigit char then Some(System.Convert.ToInt32 char - zero) else None
    
let (|Strike|_|) char =
    if char = 'X' then Some(10) else None
    
let (|Score|_|) char =
    match char with
    | Digit d -> Some(d)
    | '-' -> Some(0)
    | _ -> None

let parseScore (chars: char list): int option list =
    let fold (resultStack: int option list * char option) (char: char) : int option list * char option =
        let result, stack = resultStack
        match stack, char with
        | Some (Score d), 'X' -> (Some(10)::Some(d)::result, None)
        | None, 'X' -> (Some(10)::result, None)
        | Some (Score d), '/' -> (Some(10-d)::Some(d)::result, None)
        | None, Score _ -> (result, Some(char))
        | Some (Score s), _ -> (Some(s)::result, Some(char))
        | _, _ -> (None::result, None)
        
    let result, stack = List.fold fold ([], None) chars
    match stack with
    | Some s -> fold (result, None) s |> fst |> List.rev
    | None -> result |> List.rev

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

type Frame =
    | Open of int * int
    | Spare of int * int
    | Strike of int
    | Final of Frame

let parseFrames (scores: int list): Frame list =
    let fold (resultStack: Frame list * int option) (score: int) : Frame list * int option =
        let result, stack = resultStack
        match stack with
        | Some s when s + score = 10 -> (Spare(s, score)::result, None)
        | Some s -> (Open(s, score)::result, None)
        | None when score = 10 -> (Strike(score)::result, None)
        | None -> (result, Some(score))
        
    let result, stack = List.fold fold ([], None) scores
    SHOW (result |> List.rev, stack)
    result |> List.rev
    
type strikeCount = Zero | One | Two
    
let countScore (scores: int list): int =
    let frames = parseFrames scores
    
    let fold acc frame =
        let result, strikeCount, lastFrame = acc
        SHOW (result, strikeCount, lastFrame, frame)
        match frame, strikeCount, lastFrame with
        | Strike s, Two, _ -> (result + 30, Two, frame)
        | Strike s, One, _ -> (result + 20, Two, frame)
        | Strike s, Zero, Spare _ -> (result + 2 * 10, One, frame)
        | Strike s, Zero, _ -> (result + 10, One, frame)
        | Open(i, j), _, Spare _ -> (result + 2 * i + j, Zero, frame)
        | Spare(i, j), _, Spare _ -> (result + 2 * i + j, Zero, frame)
        | Spare _, _, _ -> (result + 10, Zero, frame)
        | Open(i, j), _, _ -> (result + i + j, Zero, frame)
        
        
    let result, s, l = List.fold fold (0, Zero, Open(0, 0)) frames
    SHOW (result, s, l)
    SHOW "-------"
    result
      
        

// let ``exercise 3.2`` =
//     [ [ 10
//         10
//         10
//         10
//         10
//         10
//         10
//         10
//         10
//         10
//         10
//         10 ]
//       [ 9
//         0
//         9
//         0
//         9
//         0
//         9
//         0
//         9
//         0
//         9
//         0
//         9
//         0
//         9
//         0
//         9
//         0
//         9
//         0 ]
//       [ 5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5
//         5 ]
//       [ 10
//         9
//         1
//         5
//         5
//         7
//         2
//         10
//         10
//         10
//         9
//         0
//         8
//         2
//         9
//         1
//         10 ] ]
//     |> List.map countScore
// //** #### Value of ``exercise 3.2`` *)
// SHOW ``exercise 3.2``

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
let bowlingScore (score: string): int option =
    SHOW score
    match parseScore (Seq.toList score) |> sequenceOpts with
    | Some list -> Some(countScore list)
    | None -> None

let ``bonus homework`` =
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

//** #### Value of ``bonus homework`` *)
SHOW ``bonus homework``

//EXPECTED RESULTS   
//[Some 300; Some 90; Some 100; Some 111; Some 92;
//   Some 90; Some 111; Some 150; Some 155; Some 187]

//////////////////////////////////////////////////////////////
/// Indeks:
/// Imię:
/// Nazwisko:
/// 
/// Podsumowanie zalizowanych zadan: