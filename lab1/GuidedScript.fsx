// This F# dojo is directly inspired by the
// Digit Recognizer competition from Kaggle.com:
// http://www.kaggle.com/c/digit-recognizer
// The datasets below are simply shorter versions of
// the training dataset from Kaggle.

// The goal of the dojo will be to
// create a classifier that uses training data
// to recognize hand-written digits, and
// evaluate the quality of our classifier
// by looking at predictions on the validation data.

// This file provides some guidance through the problem:
// each section is numbered, and
// solves one piece you will need. Sections contain
// general instructions,
// [ YOUR CODE GOES HERE! ] tags where you should
// make the magic happen, and
// <F# QUICK-STARTER> blocks. These are small
// F# tutorials illustrating aspects of the
// syntax which could come in handy. Run them,
// see what happens, and tweak them to fit your goals!


// 0. GETTING READY

// <F# QUICK-STARTER>
// With F# Script files (.fsx) and F# Interactive,
// you can "live code" and see what happens.

// Try typing let x = 42 in the script file,
// right-click and select "Execute in interactive".
// You can also hit ALT + ENTER on a line to send it to
// F# interactive (FSI). This also works in VS Code.

// let "binds" the value on the right to a name.

// Now execute the following lines in FSI (highlight both
// lines and execute them "together"):
let greet name = printfn "Hello, %s" name

// let also binds a name to a function.
// greet is a function with one argument, name.
// You should be able to call this function by entering
// the following and sending it to FSI:
// greet "World"
// </F# QUICK-STARTER>

// Two data files are included in the same place you
// found this script:
// trainingsample.csv, a file that contains 5,000 examples, and
// validationsample.csv, a file that contains 500 examples.
// The first file will be used to train your model, and the
// second one to validate the quality of the model.

// 1. GETTING SOME DATA

// First let's read the contents of "trainingsample.csv"

// We will need System and System.IO to work with files,
// let's right-click / run in interactive,
// to have these namespaces loaded:

open System
open System.IO

// the following might come in handy:
//File.ReadAllLines(path)
// returns an array of strings for each line

// [ YOUR CODE GOES HERE! ]

let trainingData = File.ReadAllLines("trainingsample.csv")

// 2. EXTRACTING COLUMNS

// Break each line of the file into an array of string,
// separating by commas, using Array.map

// <F# QUICK-STARTER>
// Array.map quick-starter:
// Array.map takes an array, and transforms it
// into another array by applying a function to it.
// Example: starting from an array of strings:
let strings = [| "Machine"; "Learning"; "with"; "F#"; "is"; "fun" |]

// We can transform it into a new array,
// containing the length of each string:
let lengths = Array.map (fun (s: string) -> s.Length) strings
// The exact same operation above can be
// done using the forward pipe operator,
// which makes it look nicer:
let lengths2 = strings |> Array.map (fun s -> s.Length)
// </F# QUICK-STARTER>

// The following function might help
let csvToSplit = "1,2,3,4,5"
let splitResult = csvToSplit.Split(',')

// [ YOUR CODE GOES HERE! ]

let trainingRows = trainingData |> Array.map (fun line -> line.Split(','))

// 3. CLEANING UP HEADERS

// Did you note that the file has headers? We want to get rid of it.

// <F# QUICK-STARTER>
// Array slicing quick starter:
// Let's start with an Array of ints:
let someNumbers = [| 0..10 |] // create an array from 0 to 10
// You can access Array elements by index:
let first = someNumbers.[0]
// You can also slice the array:
let twoToFive = someNumbers.[1..4] // grab a slice
let upToThree = someNumbers.[..2]
// </F# QUICK-STARTER>


// [ YOUR CODE GOES HERE! ]

let headers = trainingRows.[0]
let stringData = trainingRows.[1..]

// 4. CONVERTING FROM STRINGS TO INTS

// Now that we have an array containing arrays of strings,
// and the headers are gone, we need to transform it
// into an array of arrays of integers.
// Array.map seems like a good idea again :)

// The following might help:
let castedInt = (int) "42"
// or, alternatively:
let convertedInt = Convert.ToInt32("42")


// [ YOUR CODE GOES HERE! ]

let intData =
    stringData |> Array.map (fun line -> line |> Array.map Convert.ToInt32)


// 5. CONVERTING ARRAYS TO RECORDS

// Rather than dealing with a raw array of ints,
// for convenience let's store these into an array of Records

// <F# QUICK-STARTER>
// Record quick starter: we can declare a
// Record (a lightweight, immutable class) type that way:
type Example = { Label: int; Pixels: int[] }
// and instantiate one this way:
let example = { Label = 1; Pixels = [| 1; 2; 3 |] }
// </F# QUICK-STARTER>


// [ YOUR CODE GOES HERE! ]

let data =
    intData
    |> Array.map (fun line ->
        { Label = line.[0]
          Pixels = line.[1..] })

let loadAndTransformData (fileName: string) =
    let rows = File.ReadAllLines(fileName) |> Array.map (fun line -> line.Split(','))

    rows.[1..]
    |> Array.map (fun line -> line |> Array.map Convert.ToInt32)
    |> Array.map (fun line ->
        { Label = line.[0]
          Pixels = line.[1..] })

// 5.1 VISUALISING THE DATA

// You can visualise the data for an observation using
// a helper function in this repository. First, you can
// "load" the helper module using the #load FSI command:
//#load "Helper.fsx"
// This module contains a function, prettyPrint, which
// prints an ASCII art representation of the number
// directly into the REPL e.g.
// prettyPrint example.Pixels

// [ YOUR CODE GOES HERE! ]

#load "Helper.fsx"

// prettyPrint data.[0].Pixels

// 6. COMPUTING DISTANCES

// We need to compute the "distance" between images
// Math reminder: the euclidean distance is
// distance [ x1; y1; z1 ] [ x2; y2; z2 ] =
// sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2))

// <F# QUICK-STARTER>
// Array.map2 could come in handy here.
// Suppose we have 2 arrays:
let point1 = [| 0; 1; 2 |]
let point2 = [| 3; 4; 5 |]
// Array.map2 takes 2 arrays at a time
// and maps pairs of elements, for instance:
let map2Example = Array.map2 (fun p1 p2 -> p1 + p2) point1 point2
// This simply computes the sums for point1 and point2,
// but we can easily turn this into a function now:
let map2PointsExample (P1: int[]) (P2: int[]) = Array.map2 (fun p1 p2 -> p1 + p2) P1 P2
// </F# QUICK-STARTER>

// Having a function like
let distance (p1: int[]) (p2: int[]) = 42
// would come in very handy right now,
// except that in this case,
// 42 is likely not the right answer

// [ YOUR CODE GOES HERE! ]

let euclideanDistance (p1: int[]) (p2: int[]) =
    let diff = Array.map2 (fun x y -> x - y) p1 p2
    let squared = Array.map (fun x -> x * x) diff
    Array.sum squared |> float |> sqrt

// 7. WRITING THE CLASSIFIER FUNCTION

// We are now ready to write a classifier function!
// The classifier should take a set of pixels
// (an array of ints) as an input, search for the
// closest example in our sample, and use
// that value as the prediction.

// <F# QUICK-STARTER>
// Array.minBy can be handy here, to find
// the closest element in the Array of examples.
// Suppose we have an Array of Example:
let someData =
    [| { Label = 0; Pixels = [| 0; 1 |] }
       { Label = 1; Pixels = [| 9; 2 |] }
       { Label = 2; Pixels = [| 3; 4 |] } |]
// We can find for instance
// the element with largest first pixel
let findThatGuy = someData |> Array.maxBy (fun x -> x.Pixels.[0])
// </F# QUICK-STARTER>


// <F# QUICK-STARTER>
// F# and closures work very well together
let immutableValue = 42

let functionWithClosure (x: int) =
    if
        x > immutableValue // using outside value
    then
        true
    else
        false
// </F# QUICK-STARTER>


// The classifier function should probably
// look like this - except that this one will
// classify everything as a 0:
let classify (unknown: int[]) =
    // do something smart here
    // like find the Example with
    // the shortest distance to
    // the unknown element...
    // and use the training examples
    // in a closure...
    0

// [ YOUR CODE GOES HERE! ]

let findKNearest (k: int) (unknown: int[]) (distanceFunction: int[] -> int[] -> float) =
    data
    |> Array.map (fun example -> (example.Label, distanceFunction example.Pixels unknown))
    |> Array.sortBy snd
    |> Array.take k

let knnClassifier (k: int) (unknown: int[]) (distanceFunction: int[] -> int[] -> float) =
    findKNearest k unknown distanceFunction
    |> Seq.countBy fst
    |> Seq.maxBy snd
    |> fst

// 8. EVALUATING THE MODEL AGAINST VALIDATION DATA

// Now that we have a classifier, we need to check
// how good it is.
// This is where the 2nd file, validationsample.csv,
// comes in handy.
// For each Example in the 2nd file,
// we know what the true Label is, so we can compare
// that value with what the classifier says.
// You could now check for each 500 example in that file
// whether your classifier returns the correct answer,
// and compute the % correctly predicted.


// [ YOUR CODE GOES HERE! ]

let validationData = loadAndTransformData "validationsample.csv"

let manhattanDistance (p1: int[]) (p2: int[]) =
    Array.map2 (fun x y -> abs(x - y)) p1 p2 |> Array.sum |> float

let minkowskiDistance (p: int) (p1: int[]) (p2: int[]) =
    let diff = Array.map2 (fun x y -> float(abs(x - y)) ** p ) p1 p2
    let sum = float(Array.sum diff)
    sum ** (1.0 / float(p))

let calculateValidationPercentage (predicted: (int * int)[]) =
    let comparison =
        predicted
        |> Array.map (fun (label, prediction) -> Convert.ToInt32((label = prediction)))

    let floatSum = comparison |> Array.sum |> float
    let floatLength = comparison.Length |> float

    floatSum / floatLength * 100.0


let validate (data: Example[]) (k: int) (distanceFunction: int[] -> int[] -> float) =
    data
    |> Array.map (fun example -> (example.Label, knnClassifier k example.Pixels distanceFunction))
    |> calculateValidationPercentage

let validateValidationData (k: int) (distanceFunction: int[] -> int[] -> float) =
    validate validationData k distanceFunction


validateValidationData 3 euclideanDistance
validateValidationData 3 manhattanDistance
validateValidationData 3 (minkowskiDistance 3)

///////podsumowanie
// 1NN Euclidean: 94.400000
// 2NN Euclidean: 94.400000
// KNN Euclidean for k = 10: 93.600000
// KNN Manhattan for k = 10: 92.400000
// KNN Minkowski for k = 10 and p = 3: 93.800000

// 1NN - Euclidean
let euclidean1Result = validateValidationData 1 euclideanDistance
printfn $"1NN Euclidean: %f{euclidean1Result}"

// 2NN - Euclidean
let euclidean2Result = validateValidationData 2 euclideanDistance
printfn $"2NN Euclidean: %f{euclidean2Result}"

let k = 10
// KNN - Euclidean - k = 10
let euclidean10Result = validateValidationData k euclideanDistance
printfn $"KNN Euclidean for k = 10: %f{euclidean10Result}"

// KNN - Manhattan - k = 10
let manhattan10Result = validateValidationData k manhattanDistance
printfn $"KNN Manhattan for k = 10: %f{manhattan10Result}"

// KNN - Minkowski - k = 10 - p = 3
let p = 3
let minkowski10Result = validateValidationData k (minkowskiDistance p)
printfn $"KNN Minkowski for k = 10 and p = 3: %f{minkowski10Result}"
