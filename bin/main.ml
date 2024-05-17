open Base

type color =
  | Red
  | Green
  | Blue
  | Yellow
  | RGB of int * int * int

type 'a option =
  | None
  | Some of 'a

type expr =
  | Num of int
  | Add of expr * expr
  | Substruct of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr
  | Pow of expr * expr

let rec evaluate e =
  let rec ev e =
    match e with
    | Num x -> x
    | Add (e, e') -> ev e + ev e'
    | Substruct (e, e') -> ev e - ev e'
    | Multiply (e, e') -> ev e * ev e'
    | Divide (e, e') -> ev e / ev e'
    | Pow (e, e') -> ev e ** ev e'
  in
  try Some (ev e) with
  | Division_by_zero -> None
;;

type rect =
  | Rectangle of int * int
  | Square of int

let area rect =
  match rect with
  | Rectangle (w, l) -> w * l
  | Square a -> a * a
;;

let rotate rect =
  match rect with
  | Rectangle (w, l) -> if w > l then Rectangle (l, w) else rect
  | rest -> rest
;;

let pack rectl =
  List.map rectl ~f:rotate
  |> List.sort ~compare:(fun x y ->
    match area x, area y with
    | x, y when x < y -> -1
    | x, y when x > y -> 1
    | _ -> 0)
;;

type 'a sequence =
  | Nil
  | Cons of 'a * 'a sequence

let rec take sequence n =
  if n <= 0
  then Nil
  else (
    match sequence with
    | Nil -> Nil
    | Cons (a, a') -> Cons (a, take a' (n - 1)))
;;

let rec drop sequence n =
  if n <= 0
  then sequence
  else (
    match sequence with
    | Nil -> Nil
    | Cons (_, a') -> drop a' (n - 1))
;;

let rec map f seq =
  match seq with
  | Nil -> Nil
  | Cons (a, a') -> Cons (f a, map f a')
;;

let () = ()
