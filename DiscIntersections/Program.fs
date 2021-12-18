(*
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*)

open System

let mapniFold folder acc list =
  let n = (List.length list) - 1

  let rec mapniFoldAcc acc i x =
    match x with
    | [] -> acc
    | head :: tail ->
      mapniFoldAcc (folder acc i head n) (i + 1) tail

  mapniFoldAcc acc 0 list

let count x =
  let (left, right) = x
  List.countBy id left,
  List.countBy id right

let sort x =
  let (left, right) = x
  List.sortBy fst left,
  List.sortBy fst right

let align x =
  let (left : (int*int) list, right : (int*int) list) = x

  let rec alignMiddle list =
    match list with
    | [] -> list
    | a :: b :: tail when (fst a + 1) <> fst b ->
      alignMiddle (a :: (fst a + 1, 0) :: b :: tail)
    | head :: tail -> head :: alignMiddle tail

  let alignSides min max list =
    let left = fst (List.head list)
    let right = fst (List.last list)

    let prefix = List.init (left - min) (fun x -> (x, 0))
    let postfix = List.init (max - right) (fun x -> (right + x + 1, 0))

    prefix @ list @ postfix

  let min = Math.Min (fst (List.head left), fst (List.head right))
  let max = Math.Max (fst (List.last left), fst (List.last right))

  alignMiddle (alignSides min max left),
  alignMiddle (alignSides min max right)

let collect x =
  let (left, right) = x
  List.mapi2 (fun i x y -> i, snd x, snd y) left right

let toList (expr : string) =
  List.ofArray (expr.Split ',')
  |> List.map uint
  |> List.map int

let solve (input : int list) : int =
  let makeTuple acc i x n =
    let (left, right) = acc
    Math.Clamp ((i - x), 0, n) :: left,
    Math.Clamp ((i + x), i, n) :: right

  let calcIntersections acc x =
    let (alive, intersections) = acc
    let (_, created, destroyed) = x

    let withOld = alive * created
    let betweenNew = created * (created - 1) / 2

    alive + created - destroyed,
    intersections + withOld + betweenNew

  mapniFold makeTuple ([],[]) input
  |> count
  |> sort
  |> align
  |> collect

  |> List.fold calcIntersections (0,0)
  |> snd

[<EntryPoint>]
let main argv =
  let mutable isRunning = true

  [
    "--- NumberOfDiscIntersections solution written in F# ---"
    "Enter a list of disc radiuses (e.g. 1,5,2,1,4,0)"
    "Type \"q\" or \"quit\" to exit"
  ]
  |> List.iter (printfn "%s")

  while isRunning do
    printf ": "
    match Console.ReadLine () with
    | "q" | "quit" -> isRunning <- false
    | expression when String.IsNullOrWhiteSpace expression -> ()
    | expression ->
      try
        expression
        |> toList
        |> solve
        |> printfn "Answer: %A"
      with
      | :? FormatException -> printfn "Error: Invalid input given"
      | :? OverflowException -> printfn "Error: Number is too big or too small"

  0