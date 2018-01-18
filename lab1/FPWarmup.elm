module FPWarmup exposing (..)

import List
import Result exposing (..)
import Maybe exposing (..)

firstDigit : Int -> Int -> (Int, Int)
firstDigit n c =
  let num =
    n // 10
  in
    if num == 0 then
      (n, c)
    else
      firstDigit num (c + 1)

remainingDigits : Int -> Int -> Int
remainingDigits num c = num % (10^c)

digitsOfInts : Int -> List Int
digitsOfInts n =
  if n < 0 then
    []
  else
    let (first, c) =
      firstDigit n 0
    in
      if c /= 0 then
        first :: (remainingDigits n c
        |> digitsOfInts)
      else
        List.singleton first

digitalRoot : Int -> Int
digitalRoot n =
  let l =
     digitsOfInts n
  in
    if List.length l > 1 then
      List.sum l
      |> digitalRoot
    else
      case List.head l of
        Just n -> n
        _ -> 0 --this should never get hit

additivePersistence: Int -> Int
additivePersistence n =
  let l =
    digitsOfInts n
  in
    if List.length l > 1 then
      (List.sum l |> additivePersistence) + 1
    else
      0

take : Int -> List a -> Result String (List a)
take num list =
  if num < 0 then
    Err "negative index"
  else if num > List.length list then
    Err "not enough elements"
  else
    case (num, list) of
      (0, _) -> Ok []
      (_, []) -> Ok []
      (n, x::xs) -> case (take (n-1) xs) of
          Ok result -> Ok (x :: result)
          Err _ as err -> err


subsequences : List a -> List (List a)
subsequences list =
  case list of
    [] -> [[]]
    x::xs ->
      let ss = subsequences xs in
        let ssp = List.map (\a -> x :: a) ss in
          List.append ss ssp
