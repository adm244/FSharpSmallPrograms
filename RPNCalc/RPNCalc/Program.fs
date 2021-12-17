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