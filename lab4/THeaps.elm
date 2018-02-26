module THeaps exposing
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge, pathTo)

type Tree a = Empty | Node a (Tree a) (Tree a)
type Heap a = Heap (Int, Tree a)

------------------------------------------------------------------------------
-- Helper functions

type Dir = Left | Right

pathTo : Int -> List Dir
pathTo =
  let foo i =
    if i == 0 then []
    else if rem i 2 == 1 then Left :: foo (parentIndex i)
    else Right :: foo (parentIndex i)
  in
  List.reverse << foo

parentIndex i = (i-1) // 2

unwrapNode t =
  case t of
    Node x left right -> (x, left, right)
    Empty             -> Debug.crash "unwrapNode"

------------------------------------------------------------------------------

empty : Heap comparable
empty = Heap (0, Empty)

isEmpty : Heap comparble -> Bool
isEmpty h =
  case h of
    Heap (0, _) -> True
    _ -> False

findMin : Heap comparable -> Maybe comparable
findMin h =
  case h of
    Heap (0, _) -> Nothing
    Heap (_, (Node a _ _) ) -> Just a

insert : comparable -> Heap comparable -> Heap comparable
insert x h =
  case h of
    Heap (n, t) ->
      if n==0
      then Heap (1, Node x Empty Empty)
      else Heap (n+1, )

insertAndBubbleUp : comparable -> List Dir -> Tree comparable -> Tree comparable
insertAndBubbleUp x dirList t =



deleteMin : Heap comparable -> Maybe (comparable, Heap comparable)
deleteMin h = Debug.crash "TODO"

------------------------------------------------------------------------------

merge : Heap comparable -> Heap comparable -> Heap comparable
merge _ _ = Debug.crash "merge: not implemented"

------------------------------------------------------------------------------
