module BraunTree where

data Tree a = Empty | Node a (Tree a) (Tree a)

{-@
measure nodes
nodes :: Tree a -> Nat
@-}
nodes :: Tree a -> Int
nodes Empty = 0
nodes (Node x l r) = 1 + nodes (l) + nodes (r)

{-@
measure braun
braun :: Tree a -> Bool
@-}
braun :: Tree a -> Bool
braun Empty = True
braun (Node _ l r) =
    (nl == nr || nl == nr+1) && braun l && braun r
    where nl = nodes l
          nr = nodes r

{-@
halve :: l:[a] -> {t:([a], [a]) | (len (fst t)) == (div ((len l)+1) 2) &&
                                  (len (snd t)) == (div (len l) 2) }
@-}
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve (x:xs) = (x:evens, odds)
    where (odds, evens) = halve xs

{-@
createBraunTree :: l:[a] -> {t: Tree a | braun t && nodes t == len l}
@-}
createBraunTree :: [a] -> Tree a
createBraunTree [] = Empty
createBraunTree (x:xs) = 
    Node x (createBraunTree odds) (createBraunTree evens)
    where (odds,evens) = halve xs

{-@
update :: t:{s: Tree a | braun s} -> {i: Int | i >= 0 && i < nodes t} -> a -> {s: Tree a | braun s && nodes s == nodes t}
@-}
update :: Tree a -> Int -> a -> Tree a
update t@(Node x l r) i v | i == 0 = Node v l r
                          | mod i 2 == 1 = Node x (update l (div i 2) v) r
                          | mod i 2 == 0 = Node x l (update r (div i 2 - 1) v)

{-@
pop :: t:{s: Tree a | braun s && nodes s > 0} -> {s: Tree a | braun s && nodes s == (nodes t) - 1}
@-}
pop :: Tree a -> Tree a
pop (Node x l r) = merge l r

{-@
merge :: t1:{s: Tree a | braun s} -> t2:{s: Tree a | braun s && (nodes t1 == nodes s || nodes t1 == nodes s+1)} -> {s: Tree a | braun s && nodes s == nodes t1 + nodes t2}
@-}
merge :: Tree a -> Tree a -> Tree a
merge Empty t2 = t2
merge (Node x l r) t2 = Node x t2 (merge l r)

{-@
push :: t:{s: Tree a | braun s} -> a -> {s: Tree a | braun s && nodes s == nodes t + 1}
@-}
push :: Tree a -> a -> Tree a
push Empty x = Node x Empty Empty
push (Node y l r) x = Node x (push r y) l
