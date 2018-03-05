module LHeaps exposing (..)

{-- Copied from LeftistHeap.elm from class. DO NOT MODIFY. -------------}

type alias Rank = Int

type Heap a = E | T Rank a (Heap a) (Heap a)

rank : Heap a -> Rank
rank h =
  case h of
    E         -> 0
    T r _ _ _ -> r

makeT : a -> Heap a -> Heap a -> Heap a
makeT x h1 h2 =
  let (left,right) =
    if rank h1 >= rank h2
      then (h1, h2)
      else (h2, h1)
  in
  T (1 + rank right) x left right

merge : Heap comparable -> Heap comparable -> Heap comparable
merge h1 h2 = case (h1, h2) of
  (_, E) -> h1
  (E, _) -> h2
  (T _ x1 left1 right1, T _ x2 left2 right2) ->
    if x1 <= x2
      then makeT x1 left1 (merge right1 h2)
      else makeT x2 left2 (merge h1 right2)

insert : comparable -> Heap comparable -> Heap comparable
insert x h = merge (T 1 x E E) h

singletonHeap : comparable -> Heap comparable
singletonHeap x = T 1 x E E


{------------------------------------------------------------------------}

fromList : List comparable -> Heap comparable
fromList l = case l of
  [] -> E
  _ -> case (makePass <| List.map singletonHeap l) of
    [x] -> x
    _ -> Debug.crash "Should not hit this case"

mergePairs : List (Heap comparable) -> List (Heap comparable)
mergePairs l = case l of
  x1 :: x2 :: [] -> [merge x1 x2]
  _ -> Debug.crash "TODO"

makePass : List (Heap comparable) -> List (Heap comparable)
makePass l = case l of
  [x] -> l
  x1 :: x2 :: xs -> makePass <| (++) xs <| mergePairs [x1,x2]
  _ -> Debug.crash "TODO"
