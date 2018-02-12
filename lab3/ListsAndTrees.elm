module ListsAndTrees exposing ( .. )


------------------------------------------------------------------------------
-- Problem 1
------------------------------------------------------------------------------

suffixes : List a -> List (List a)
suffixes xs =
  case xs of
    [] -> [[]]
    _ :: t -> xs :: suffixes t


------------------------------------------------------------------------------
-- Problem 2
------------------------------------------------------------------------------

type Tree a = Empty | Node a (Tree a) (Tree a)

mem : comparable -> Tree comparable -> Bool
mem x tree =
  case tree of
    Empty -> False
    Node d l r ->
      if x == d then
        True
      else
        mem x l || mem x r


fullTree : a -> Int -> Tree a
fullTree x h =
  case h of
    0 -> Empty
    _ ->
      let child =
        fullTree x (h-1)
      in
        Node x child child

balancedTree : a -> Int -> Tree a
balancedTree _ _ =
  -- TODO
  Empty

create2 : a -> Int -> (Tree a, Tree a)
create2 a s =
  let helper n =
    if n == 0 then
      (Empty, Node a Empty Empty)
    else if odd n then
      let (t1, t2) = helper (n//2) in (Node a t1 t1, Node a t1 t2)
    else
      let (t1, t2) = helper (n//2 - 1) in (Node a t1 t2, Node a t2 t2)
  in helper s




balancedTrees : a -> Int -> List (Tree a)
balancedTrees _ _ =
  -- TODO
  []

completeTrees : a -> Int -> List (Tree a)
completeTrees _ _ =
  -- TODO
  []

almostCompleteTrees : a -> Int -> List (Tree a)
almostCompleteTrees _ _ =
  -- TODO
  []


------------------------------------------------------------------------------
-- Helpers/Misc functions
------------------------------------------------------------------------------

insert : comparable -> Tree comparable -> Tree comparable
insert item tree =
  case tree of
    Empty -> Node item Empty Empty
    Node d l r ->
      if item >= d then
        Node d l (insert item r)
      else
        Node d (insert item l) r

treeSize : Tree a -> Int
treeSize tree =
  case tree of
    Empty -> 0
    Node _ l r -> 1 + (treeSize l) + (treeSize r)

even : Int -> Bool
even n = (n % 2) == 0

odd : Int -> Bool
odd n = (n % 2) == 1
