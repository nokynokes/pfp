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
    _ -> Debug.crash "TODO"

insert : comparable -> Heap comparable -> Heap comparable
insert x h =
  case h of
    Heap (n, t) ->
      if n==0
      then Heap (1, Node x Empty Empty)
      else Heap (n+1, insertAndBubbleUp x (pathTo (parentIndex n)) t)

insertAndBubbleUp : comparable -> List Dir -> Tree comparable -> Tree comparable
insertAndBubbleUp x dirList t =
  case (dirList, (unwrapNode t)) of
    ([], (d, l, r)) ->
      if d < x then
        case (l,r) of
          (Empty, _) -> Node d (Node x Empty Empty) r
          (_, Empty) -> Node d l (Node x Empty Empty)
          _ -> Debug.crash "should never hit this case"
      else
        case (l,r) of
          (Empty, _) -> Node x (Node d Empty Empty) r
          (_, Empty) -> Node x l (Node d Empty Empty)
          _ -> Debug.crash "should never hit this case"
    (Right :: t, (d, l ,r)) ->
      let rTree = insertAndBubbleUp x t r in
        case unwrapNode rTree of
          (xp, lp, rp) ->
            if d < xp then
              Node d l rTree
            else
              Node xp l (Node d lp rp)
    (Left :: t, (d , l, r)) ->
      let lTree = insertAndBubbleUp x t l in
        case unwrapNode lTree of
          (xp, lp, rp) ->
            if d < xp then
              Node d lTree r
            else
              Node xp (Node d lp rp) r







deleteMin : Heap comparable -> Maybe (comparable, Heap comparable)
deleteMin h = Debug.crash "TODO"

------------------------------------------------------------------------------

merge : Heap comparable -> Heap comparable -> Heap comparable
merge _ _ = Debug.crash "merge: not implemented"

------------------------------------------------------------------------------
