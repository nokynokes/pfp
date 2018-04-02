module BHeaps exposing
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge)

type Tree a = Node a (List (Tree a))
type alias Rank = Int
type Heap a = Heap (List (Rank, Tree a))

{-- Internal Helpers ----------------------------------------------------}

rank : (Rank, Tree comparable)
rank (r, _) = r

link : (Rank, Tree a) -> (Rank, Tree a) -> (Rank, Tree a)
link node1 node2 =
  case (node1, node2) of
    ((r, Node x1 xs1) , (_, Node x2 xs2)) ->
      if x1 <= x2
      then (r + 1, Node x1 node2::xs1)
      else (r + 1, Node x2 node1::xs2)


{-- External Interface --------------------------------------------------}

empty : Heap comparable
empty = WrapHeap []

isEmpty : Heap comparable -> Bool
isEmpty h = h == empty

insert : comparable -> Heap comparable -> Heap comparable
insert x (Heap ls) =
  let node = (0, Node x []) in
    insert_ ls node |> Heap


insert_ : List (Rank, Tree comparable) -> (Rank, Tree comparable) -> List (Rank, Tree comparable)
insert_ ls node =
  case ls of
    [] -> [(0, node)]
    x :: xs ->
      if rank node == rank x then
        insert_ xs <| link x node
      else
        node :: ls

merge : Heap comparable -> Heap comparable -> Heap comparable
merge (Heap l1) (Heap l2) = Heap <| merge_ l1 l2

merge_ : List (Rank, Tree comparable) -> List (Rank, Tree comparable) -> List (Rank, Tree comparable)
merge_ l1 l2 =
  case (l1, l2) of
    ((_,[]), _) -> l2
    (_, (_,[])) -> l1
    ((r1, x1)::xs1, (r2, x2)::xs2) ->
      if r1 < r2 then
        x1 :: merge_ xs1 l2
      else if r1 > r2 then
        x2 :: merge_ l1 xs2
      else
        insert_ (merge_ xs1 xs2) (link (r1,x1) (r2,x2)) 



findMin : Heap comparable -> Maybe comparable
findMin h =
  -- TODO
  Nothing

deleteMin : Heap comparable -> Maybe (comparable, Heap comparable)
deleteMin h =
  -- TODO
  Nothing
