let SHOW x = printf "%A\n" x

// replace the following Symbol by the valid implementation

//// Copied from exercises.fsx

type Operator =
    | Plus
    | Minus
    | Multiply
    | Divide

type Bracket =
    | Open
    | Close

type Symbol =
    | NumSymbol of int
    | OpSymbol of Operator

type InfixSymbol =
    | OnpSymbol of Symbol
    | BracketSymbol of Bracket

let apply (operator: Operator) (left: int) (right: int) : int =
    match operator with
    | Plus -> left + right
    | Minus -> left - right
    | Multiply -> left * right
    | Divide -> left / right

let parseNumber (value: string) : int option =
    match System.Int32.TryParse(value) with
    | true, n -> Some n
    | false, _ -> None

let parseSymbol (token: string) : Symbol option =
    match token with
    | "+" -> Some((OpSymbol Plus))
    | "-"
    | "−" -> Some((OpSymbol Minus))
    | "*"
    | "×" -> Some((OpSymbol Multiply))
    | "/" -> Some((OpSymbol Divide))
    | _ -> parseNumber token |> Option.map NumSymbol

let parseInfixSymbol (token: string) : InfixSymbol option =
    match token with
    | "(" -> Some(BracketSymbol Open)
    | ")" -> Some(BracketSymbol Close)
    | _ -> parseSymbol token |> Option.map OnpSymbol

let splitBy (separator: char) (str: string) : string list = str.Split(separator) |> Array.toList

let rec sequenceOpts (optionals: 'a option list) : 'a list option =
    match optionals with
    | [] -> Some []
    | None :: _ -> None
    | Some h :: t -> sequenceOpts t |> Option.map (fun t -> h :: t)

let parseSymbols (expression: string) : Symbol list option =
    expression |> splitBy ' ' |> List.map parseSymbol |> sequenceOpts

let parseInfixSymbols (expression: string) : InfixSymbol list option =
    expression |> splitBy ' ' |> List.map parseInfixSymbol |> sequenceOpts

let convertFromInfix (infix: InfixSymbol) : Symbol option =
    match infix with
    | OnpSymbol s -> Some(s)
    | _ -> None

let priority (operator: InfixSymbol) : int =
    match operator with
    | BracketSymbol Open -> 0
    | OnpSymbol(OpSymbol Plus)
    | OnpSymbol(OpSymbol Minus)
    | BracketSymbol Close -> 1
    | OnpSymbol(OpSymbol Multiply)
    | OnpSymbol(OpSymbol Divide) -> 2
    | _ -> -1

//// End of copied code

// ### Homework 5.1
//Complete all the function below again but witout recursion ussage - so all the recursion code should be tail recursive
// ##Implement `computeonp` function (AiSD or [Wiki](https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)). Hint: `::` is "right-associative"

// #### --------------- Your code goes below ---------------

let computeonpTR (initialStack: int list) (symbols: Symbol list) : int option =
    let computeonpTR' (stack: int list option) (symbol: Symbol) =
        match symbol, stack with
        | NumSymbol n, Some s -> Some(n :: s)
        | OpSymbol op, Some(x :: y :: tail) -> Some(apply op y x :: tail)
        | _, _ -> None

    let finalStack = List.fold computeonpTR' (Some initialStack) symbols

    match finalStack with
    | Some [ result ] -> Some result
    | _ -> None


// test the function, e.g. `computeonpTR [] [NumSymbol 4; NumSymbol 2; OpSymbol Multiply]`
// Important!!!! Replace the None with commented out assignment o computeonp
let ``Homework 5.1``: int option =
    computeonpTR [] [ NumSymbol 4; NumSymbol 2; OpSymbol Multiply ]

// #### Value of ``exercise 5.1``
SHOW ``Homework 5.1``

// ### Homework 5.2
// ##Using `parseSymbols` and `compute`, write `onp` function

// #### --------------- Your code goes below ---------------


let onpTR (expression: string) : int option =
    parseSymbols expression |> Option.bind (computeonpTR [])

let ``Homework 5.2`` = onpTR "2 7 + 3 / 14 3 - 4 * + 3 +"

// #### Value of ``exercise 5.2``
SHOW ``Homework 5.2``

// ### Homework 5.3
// ##Implement `conv2onpTR` function (AiSD or (https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)).

// #### --------------- Your code goes below ---------------

let addToResultsUntilOpenBracket stack result =
    let fold tuple _ =
        let stack, result, openBracketFound = tuple

        match openBracketFound, stack, result with
        | true, _, _ -> (stack, result, true)
        | false, BracketSymbol Open :: t, _ -> (t, result, true)
        | false, h :: t, Some r ->
            match convertFromInfix h with
            | Some s -> (t, Some(s :: r), false)
            | None -> (t, None, false)
        | _, _, _ -> (stack, result, false)

    List.fold (fold) (stack, result, false) stack |> (fun (s, r, _) -> (s, r))


let addToResultsHigherPriority stack result symbol =
    let fold op tuple symbol =
        let stack, result, lowerPriorityFound = tuple
        let isPriorityHigher = priority symbol >= priority op

        match lowerPriorityFound, isPriorityHigher, stack, result with
        | false, true, h :: t, Some r ->
            match convertFromInfix h with
            | Some s -> (t, Some(s :: r), false)
            | None -> (t, None, false)
        | _, _, _, _ -> (stack, result, true)

    List.fold (fold symbol) (stack, result, false) stack
    |> (fun (s, r, _) -> (s, r))

let conv2onpTR' (stackAndResult: InfixSymbol list * Symbol list option) (symbol: InfixSymbol) =
    let stack, result = stackAndResult

    match symbol, stack, result with
    | _, _, None -> ([], None)
    | OnpSymbol(NumSymbol n), _, Some r -> (stack, Some(NumSymbol(n) :: r))
    | BracketSymbol Open, _, _ -> (BracketSymbol Open :: stack, result)
    | OnpSymbol(OpSymbol op), _, _ ->
        addToResultsHigherPriority stack result (OnpSymbol(OpSymbol op))
        |> (fun (s, r) -> (OnpSymbol(OpSymbol op) :: s, r))
    | BracketSymbol Close, _, _ -> addToResultsUntilOpenBracket stack result


let conv2onpTR (expression: string) : Symbol list option =
    match parseInfixSymbols expression with
    | Some symbols ->
        let (finalStack, finalResult) = List.fold conv2onpTR' ([], Some []) symbols
        let convertedStack = finalStack |> List.map convertFromInfix |> sequenceOpts

        match finalResult, convertedStack with
        | Some r, Some s -> Some((List.rev r) @ s)
        | _, _ -> None
    | None -> None

let ``Homework 5.3`` = conv2onpTR "( 2 + 5 ) * 3 - 4 * ( 16 + 5 )"

// #### Value of ``exercise 5.3``

SHOW ``Homework 5.3``

// ### Homework 5.4

// #### --------------- Your code goes below ---------------
let computeTR (expression: string) : int option =
    conv2onpTR expression |> Option.bind (computeonpTR [])

let ``Homework 5.4`` = computeTR "( 2 + 5 ) * 3 - 4 * ( ( 16 - 1 ) * 2 + 5 )"

// #### Value of ``exercise 5.4``
SHOW ``Homework 5.4``
