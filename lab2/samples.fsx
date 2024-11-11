(*
- title : Basic concepts of Functional Programming
- description : Basic concepts of Functional Programming
- author : Tomasz Heimowski, Krzysztof Manuszewski
*)

let SHOW x = printf "%A\n" x

// ##################################### SECTION 1
// ### New Stuff 1.1
// #### Let bindings
let value = 5
// #### Type inference
let stringValue = "Hi there"
let integerValue = 100
// #### Backticks names
let ``I can have spaces`` = "I really do"

// ### Example 1.1
// #### Processing an immutable value
//``System.String`` is an example of immutable type in .NET

let helloWorld = "Hello World from F# program!"
let replaced = helloWorld.Replace("o", "u")

let substring =
    replaced.Substring(0, replaced.IndexOf("#") + 1)

let ``example 1.1`` = substring.ToLower()

// #### Value of ``example 1.1``
//* include-value: ``example 1.1``
//


// #### Value of ``exercise 1.1``
// # include-value: ``exercise 1.1``

// ### New Stuff 1.2
// #### Range operator
let range = [ 28 .. 38 ]

// # include-value: range

// ---
// #### Function declaration and application
let add x y = x + y

let addResult = add 5 6

// # include-value: addResult

// #### Pipe operator
let pipeResult = 10 |> add 15 |> add 6

// # include-value: pipeResult

// #### map function

let lengths =
    [ "F#"; "is"; "the"; "best" ]
    |> List.map (fun s -> s.Length)

// # include-value: lengths
//`map` is an equivalent of `Select()` in C# LINQ

// ### Example 1.2
// #### LINQ-like list processing
//Note: a new immutable value is created in each computation step
//[C# version](http://theimowski.com/fsharp-workshops-intro/#/4/3)
let isOdd number = number % 2 = 1

let ``example 1.2`` =
    [ 2 .. 10 ]
    |> List.filter isOdd
    |> List.map string
    |> String.concat ";"

// #### Value of ``example 1.2``
// # include-value: ``example 1.2``

// ### Summary: Immutable Values

//* By **default** values in F# are immutable
//* "Pipe" operator (`|>`) is a nice syntactic sugar for writing a sequence of expressions
//* Immutable values make it easy to write concurrent code (thread safety for free)

// ### Links

// ##* [F# Values](https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/values-%5bfsharp%5d) - MSDN
// ##* [Immutability - Making your code predictable](https://fsharpforfunandprofit.com/posts/correctness-immutability/) by Scott Wlaschin

// ##################################### SECTION 2
// ## Expressions

// ### New Stuff 2.1
// #### In F# everything is an expression
// no statements - `Console.WriteLine` returns `Unit` ()
let writeLine = System.Console.WriteLine "Hello"

// in let bindings, `=` associates symbol with value
let writeLineIsUnit: bool =
    // but anywhere else, `=` means equality test
    writeLine = ()

// #include-value: ``writeLineIsUnit``

// #### If - then - else expression
let day =
    if System.DateTime.Now.DayOfWeek = System.DayOfWeek.Thursday
    then "thursday"
    else "some other day of week"

// # include-value: ``day``

// #### Binding values in inside scope
let day2 =
    let today = System.DateTime.Now.DayOfWeek
    if today = System.DayOfWeek.Thursday then "thursday" else "some other day of week"

// # include-value: ``day2``

// #### Option type
let workDay =
    let today = System.DateTime.Now.DayOfWeek
    if today
       <> System.DayOfWeek.Saturday
       && today <> System.DayOfWeek.Sunday then
        Some today
    else
        None

// # include-value: ``workDay``

// #### Dot notation, Wrapper functions
let length (value: string) = value.Length

let lengthOfWord = length "Hello word"

// # include-value: ``lengthOfWord``

// ### Example 2.1
// #### Parsing boolean value
let parseBool (value: string) =
    let lowercase = value.ToLowerInvariant()
    if lowercase = "true" then Some true
    elif lowercase = "false" then Some false
    else None

let ``example 2.1`` = parseBool "True"

// #### Value of ``example 2.1``

// # include-value: ``example 2.1``

// ### New Stuff 2.2
// #### Array literal
let intArray = [| 2; 4; 5 |]
let rangeArray = [| 10 .. 15 |]

// # include-value: ``rangeArray``

// ### Example 2.2
// #### No `return` statements. Last expression is the return value
let isPalindrome (value: string) =
    let charList = value.ToCharArray()
    let reversed = value.ToCharArray() |> Array.rev
    charList = reversed // this boolean expression returns value

let ``example 2.2`` = isPalindrome "kajak"

// #### Value of ``example 2.2``
// # include-value: ``example 2.2``

// ### Summary: Expressions
//* There are no statements only expressions in F#
//* Therefore no need for `return` keywords - last expression is return value
//* `Option` type is the preffered way to model missing values (as opposed to `null`)


// ### Links
//* [Expressions and Syntax series](https://fsharpforfunandprofit.com/series/expressions-and-syntax.html) by Scott Wlaschin
//* [The Option type - And why it is not null or nullable](https://fsharpforfunandprofit.com/posts/the-option-type/) by Scott Wlaschin

// ##################################### SECTION 3
// ## Pattern matching

// ### New Stuff 3.1
// #### Discriminated Unions - Empty cases (enum style)
type Size =
    | Small
    | Medium
    | Large

// #### Discriminated Unions - Complex cases
type Shape =
    | Square of float
    // `*` in type declarations stands for tuples
    | Rectangle of float * float
    | Circle of float

type Result =
    | Success // no string needed for success state
    | ErrorMessage of string // error message needed


// ### Example 3.1
// #### Modelling with Discriminated Union Types
type FruitType =
    | Banana
    | Apple
    | Grapefruit

type Meal =
    | Fruit of FruitType
    | Sandwich
    | FastFood of string

let ``example 3.1`` =
    [ Sandwich
      FastFood "Bar Żuławski"
      Fruit Apple ]

// #### Value of ``example 3.1``
// # include-value: ``example 3.1``

// ### New Stuff 3.2
// #### Pattern matching expression
let formatOptionalValue optionalValue =
    match optionalValue with
    | Some value -> "Value: " + value
    | None -> "No value at all!"

let formattedValues =
    [ Some "nice string"; None ]
    |> List.map formatOptionalValue

// # include-value: formattedValues

// ### Example 3.2
// #### Calculating area of `Shape` with help of pattern matching
let area shape =
    match shape with
    | Square edge -> edge ** 2.0
    | Rectangle (width, height) -> width * height
    | Circle radius -> System.Math.PI * (radius ** 2.0)

let ``example 3.2`` = area (Circle 10.0)

// #### Value of ``example 3.2``

// # include-value: ``example 3.2``

// ### New Stuff 3.3
// #### Pattern matching strings
let patternMatchString value =
    match value with
    | "Dog" -> "Animal"
    | "Cat" -> "Animal"
    | x -> "Something different"

let matchAnimal = patternMatchString "Chair"

// # include-value: matchAnimal

// #### Nested `match` expressions
let matchFloatingPoint value =
    match value with
    | "1" -> 1.0
    | "2" -> 2.0
    | x ->
        match x with
        | "1.0" -> 1.0
        | "2.5" -> 2.5
        | y -> 0.0

// ### Example 3.3
// #### Nested `match` - checking if meal is healthy
let isHealthy meal =
    match meal with
    | Fruit _ -> true
    | Sandwich _ -> false
    | FastFood restaurant ->
        // we can further use matched `restaurant` symbol
        match restaurant with
        | "Green Way" -> true
        | _ -> false

let ``example 3.3`` = FastFood "Bar Żuławski" |> isHealthy

// #### Value of ``example 3.3``
// # include-value: ``example 3.3``


// ### Summary: Pattern Matching
//* Using Discriminated Unions is a neat way to model data
//* Pattern matching is a powerful and elegant mechanism in F# for "branching" code
//* F# compiler warns when it finds unhandled cases in pattern matching

// ### Links

//* [Discriminated Unions - Adding types together](https://fsharpforfunandprofit.com/posts/discriminated-unions/) by Scott Wlaschin
//* [Pattern matching for conciseness](http://fsharpforfunandprofit.com/posts/match-expression/) by Scott Wlaschin
//* [Match expressions - The workhorse of F#](http://fsharpforfunandprofit.com/posts/match-expression/) by Scott Wlaschin
//* [Exhaustive pattern matching - A powerful technique to ensure correctness](https://fsharpforfunandprofit.com/posts/correctness-exhaustive-pattern-matching/) by Scott Wlaschin

// ##################################### SECTION 4
// ## Recursion

// ### New Stuff 4.1
// #### Recursive functions
let rec countdown counter =
    match counter with
    | 0 -> ""
    | x -> x.ToString() + ";" + countdown (counter - 1)

let counting = countdown 10

// # include-value: counting

// #### Tail-recursive functions with accumulator
let rec countdownAcc acc counter =
    match counter with
    | 0 -> acc
    | x -> countdownAcc (acc + ";" + x.ToString()) (counter - 1)

let countingAcc = countdownAcc "" 10

// # include-value: countingAcc

// #### Factorial

let rec factorial x =
    match x with
    | 1 -> 1
    | _ -> (factorial (x - 1)) * x

let factorialOf5 = factorial 5

// # include-value: factorialOf5

//
// #### Factorial - with accumulator
let rec factorialTail acc x =
    match x with
    | 1 -> acc
    | _ -> factorialTail (x * acc) (x - 1)

let factorialOf5Tail = factorialTail 1 5

// # include-value: factorialOf5Tail
// #### Pattern matching lists

let rec commaSeparated acc list =
    match list with
    | [] -> acc
    | [ single ] -> acc + "," + single
    | head :: tail -> commaSeparated (acc + "," + head) tail

let csv =
    commaSeparated "" [ "some"; "values"; "go"; "here" ]

// # include-value: csv

// #### Pattern in pattern
let rec formatOptionalInts acc ints =
    match ints with
    | [] -> acc
    | Some 0 :: rest -> formatOptionalInts (acc + " Zero") rest
    | Some x :: rest -> formatOptionalInts (acc + " " + x.ToString()) rest
    | None :: rest -> formatOptionalInts (acc + " NoValue!") rest

let optionalInts =
    formatOptionalInts "" [ Some 28; Some 0; None ]

// # include-value: optionalInts
// #### Pattern match guards (`when` keyword)
let isEvenNumber optNumber =
    match optNumber with
    | Some n when n % 2 = 0 -> true
    | _ -> false

let onlyEvenNumbers =
    [ Some 2; Some 3; Some 4; Some 5; None ]
    |> List.filter isEvenNumber

// #### Value of ``onlyEvenNumbers``
// # include-value: ``onlyEvenNumbers``

// ### Example 4.1
// #### Recursive call with "accumulators"
let rec partitionEvenOdd even odd numbers =
    match numbers with
    | [] -> (even, odd)
    | h :: tail when h % 2 = 0 -> partitionEvenOdd (h :: even) odd tail
    | h :: tail -> // guard here would result in compiler complaining
        // on incomplete pattern matching
        partitionEvenOdd even (h :: odd) tail

let ``example 4.1`` = partitionEvenOdd [] [] [ 1 .. 10 ]

// #### Value of ``example 4.1``
// # include-value: ``example 4.1``
