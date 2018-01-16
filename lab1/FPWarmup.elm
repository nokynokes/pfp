module FPWarmup exposing (digitsOfInts)

import List

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
        first :: (remainingDigits n c |> digitsOfInts)
      else
        List.singleton first
