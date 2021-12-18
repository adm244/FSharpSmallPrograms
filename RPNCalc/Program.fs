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

exception InvalidInput of string

let tokenize (input : string) : string list =
  input.Split ' '
  |> List.ofArray

let compute (operation : string) (stack : decimal list) =
  match (operation, stack) with
  | ("+", b :: a :: rest) -> (a + b) :: rest
  | ("-", b :: a :: rest) -> (a - b) :: rest
  | ("*", b :: a :: rest) -> (a * b) :: rest
  | ("/", b :: a :: rest) -> (a / b) :: rest
  | (number, rest) -> try
                        decimal number :: rest
                      with
                      | :? FormatException -> raise (InvalidInput number)

let evaluate (expression : string list) =
  let rec evaluate' (expression : string list) (stack : decimal list) =
    match expression with
    | [] -> stack
    | head :: tail -> evaluate' tail (compute head stack)

  evaluate' expression []

let calculate (input : string) : Result<decimal, string> =
  let getResult stack =
    match stack with
    | number :: [] -> Ok number
    | stack -> Error (sprintf "Operation expected. Stack: %A" stack)

  try
    input
    |> tokenize
    |> evaluate
    |> getResult
  with
  | InvalidInput str -> Error (sprintf "Invalid input: \"%s\"" str)
  | :? DivideByZeroException -> Error (sprintf "Division by zero detected")
  | :? OverflowException -> Error (sprintf "Number is too big or too small")

[<EntryPoint>]
let main argv =
  let mutable isRunning = true

  [
    "--- Reverse Polish Notation Calculator written in F# ---"
    "Enter an expression to calculate (e.g. -1 2 + 3 * or 5 1.25 /)"
    "Type \"q\" or \"quit\" to exit"
  ]
  |> List.iter (printfn "%s")

  while isRunning do
    printf ": "
    match Console.ReadLine () with
    | "q" | "quit" -> isRunning <- false
    | expression when String.IsNullOrWhiteSpace expression -> ()
    | expression ->
      match calculate expression with
      | Ok number -> printfn "Answer: %M" number
      | Error msg -> printfn "Error: %s" msg

  0