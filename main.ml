(** problem 01 **)
let rec last (xs: 'a list): 'a option =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last rest

(** problem 02 **)
let rec last_two (xs: 'a list): ('a * 'a) option =
  match xs with
  | [] -> None
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: rest -> last_two rest

(** problem 03 **)
let rec at (k: int) (xs: 'a list): 'a option =
  match xs with
  | x :: _ when k == 1 -> Some x
  | _ :: rest when k > 1 -> at (k - 1) rest
  | _ -> None

(** problem 04 **)
let rec length (xs: 'a list): int =
  match xs with
  | [] -> 0
  | _ :: rest -> 1 + length rest

let rec length_t (xs: 'a list) : int =
  let rec length_t' (xs: 'a list) (res: int): int =
    match xs with
    | [] -> res
    | _ :: rest -> length_t' rest (res + 1)
  in length_t' xs 0

(** problem 05 **)
let rec rev (xs: 'a list): 'a list =
  let rec rev' (xs: 'a list) (acc: 'a list): 'a list =
    match xs with
    | [] -> acc
    | x :: rest -> rev' rest (x :: acc)
  in rev' xs []
