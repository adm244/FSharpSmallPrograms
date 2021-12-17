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
    let left : int = fst (List.head list)
    let right : int = fst (List.last list)

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
  let a = [1;5;2;1;4;0]

  solve a
  |> printfn "%A"

  0