-- pex4.hs 
-- unKnot Haskell

-- name: Parker Evans

{- DOCUMENTATION:
-> Referenced my hw05.hs assignment for syntax clarification, and how to develop functions that need helpers
-> Referenced preflight readings, in-class notes, and haskell reference handout
-> Referenced https://www.haskell.org/tutorial/goodies.html for syntax of type signatures
-}

--isSame - if two tuples are the exact same
isSame :: (Char, Char) -> (Char, Char) -> Bool
isSame tuple1 tuple2 =
   if fst tuple1 == fst tuple2 && snd tuple1 == snd tuple2
      then True
      else False

--isTypeIPair - if two tuples are a Type I move
isTypeIPair :: (Char, Char) -> (Char, Char) -> Bool
isTypeIPair tuple1 tuple2 = 
   if fst tuple1 == fst tuple2 && snd tuple1 /= snd tuple2 --same letter, opposite crossing
      then True
      else False

--isTypeIIPair - if two tuples could constitute HALF of a TypeII move
isTypeIIPair :: (Char, Char) -> (Char, Char) -> Bool
isTypeIIPair tuple1 tuple2 = 
   if fst tuple1 /= fst tuple2 && snd tuple1 == snd tuple2 --different letters, same crossing
      then True
      else False

--pairTuples - pairs two tuples into a tuple of tuples
pairTuples :: (Char, Char) -> (Char, Char) -> ((Char, Char), (Char, Char))
pairTuples tuple1 tuple2 = (tuple1, tuple2)

--haveOppositeCrossings - if two pairs have opposite crossings
haveOppositeCrossings :: ((Char, Char), (Char, Char)) -> ((Char, Char), (Char, Char)) -> Bool
haveOppositeCrossings pair1 pair2 = 
   if (snd (fst pair1) /= snd (fst pair2)) && (snd (snd pair1) /= snd (snd pair2) ) --one pair both 'u' + other pair both 'o', or vice-versa 
      then True
      else False

--haveSameLetters - if two PAIRS have matching letters
haveSameLetters :: ((Char, Char), (Char, Char)) -> ((Char, Char), (Char, Char)) -> Bool
haveSameLetters pair1 pair2 = 
   if (fst (fst pair1) == fst (fst pair2) ) &&  (fst (snd pair1) == fst (snd pair2) ) --a/b a/b
       then True
       else if (fst (fst pair1) == fst (snd pair2) ) &&  (fst (snd pair1) == fst (fst pair2) ) --a/b b/a
         then True
         else False

--isTypeII - if two pairs are a TypeII move
isTypeII :: ((Char, Char), (Char, Char)) -> ((Char, Char), (Char, Char)) -> Bool
isTypeII pair1 pair2 = if (haveOppositeCrossings pair1 pair2) && (haveSameLetters pair1 pair2)
   then True
   else False

--removeTuple - removes a given tuple from a list of tuples
removeTupleH :: [(Char, Char)] -> (Char, Char) -> [(Char, Char)] -> [(Char, Char)]
removeTupleH tripCode tuple runningList = case tripCode of
   [] -> runningList
   (x:xs) -> if isSame x tuple
      then removeTupleH xs tuple runningList 
      else removeTupleH xs tuple (runningList ++ [x])

removeTuple :: [(Char, Char)] -> (Char, Char) -> [(Char, Char)]
removeTuple tripCode tuple = removeTupleH tripCode tuple []

--loop - appends the first tuple in a list to the end of the list
loop :: [(Char, Char)] -> [(Char, Char)]
loop tripCode = tripCode ++ (take 1 tripCode)

--inList - True if tuple is in list
inList :: [(Char, Char)] -> (Char, Char) -> Bool
inList list tuple = case list of
   [] -> False
   (x:xs) -> if x == tuple then True else inList xs tuple


--removeTuples - removes every tuple in list from tripCode
removeTuplesH :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
removeTuplesH tripCode list runningList = case tripCode of
   [] -> runningList
   (x:xs) -> if inList list x
      then removeTuplesH xs list runningList
      else removeTuplesH xs list (runningList ++ [x])

removeTuples :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
removeTuples tripCode list = removeTuplesH tripCode list []

--existsTypeI - True if Type I move available in tripCode
existsTypeI :: [(Char, Char)] -> Bool
existsTypeI tripCode = case tripCode of
   [] -> False
   [_] -> False
   (x:xs) -> if isTypeIPair x (head xs)
      then True
      else existsTypeI xs

--getTypeIPair - returns List of first Type I tuples
getTypeIPair :: [(Char, Char)] -> [(Char, Char)]
getTypeIPair tripCode = case tripCode of
   [] -> []
   [_] -> []
   (x:xs) -> if isTypeIPair x (head xs)
      then [x, head xs]
      else getTypeIPair xs

--typeIIMatchInList - True if there's a Type II match for pair in pairList
typeIIMatchInList :: [((Char, Char), (Char, Char))] -> ((Char, Char), (Char, Char)) -> Bool
typeIIMatchInList pairList pair = case pairList of
   [] -> False
   (x:xs) -> if isTypeII x pair
      then True
      else typeIIMatchInList xs pair

--getTypeIIMatchInList - gets TypeII match to pair from pairList
getTypeIIMatchInList :: [((Char, Char), (Char, Char))] -> ((Char, Char), (Char, Char)) -> [(Char, Char)]
getTypeIIMatchInList pairList pair = case pairList of
   [] -> []
   (x:xs) -> if isTypeII x pair
      then [fst pair, snd pair, fst x, snd x]
      else getTypeIIMatchInList xs pair

--getTypeIIPairs - returns pairList of all Type II pairs in tripCode
getTypeIIPairsH :: [(Char, Char)] -> [((Char, Char), (Char, Char))] -> [((Char, Char), (Char, Char))]
getTypeIIPairsH tripCode runningList = case tripCode of
   [] -> runningList
   [_] -> runningList
   (x:xs) -> if isTypeIIPair x (head xs)
      then getTypeIIPairsH xs (runningList ++ [(x, (head xs))] )
      else getTypeIIPairsH xs runningList

getTypeIIPairs :: [(Char, Char)] -> [((Char, Char), (Char, Char))]
getTypeIIPairs tripCode = getTypeIIPairsH tripCode []

--isTypeIIPairList - True if there's a Type II move in a PairList
isTypeIIPairListH :: [((Char, Char), (Char, Char))] -> [((Char, Char), (Char, Char))] -> Bool
isTypeIIPairListH pairList constPairList = case pairList of
   [] -> False
   (x:xs) -> if typeIIMatchInList constPairList x
      then True
      else isTypeIIPairListH xs constPairList

isTypeIIPairList :: [((Char, Char), (Char, Char))] -> Bool
isTypeIIPairList pairList = isTypeIIPairListH pairList pairList

--getTypeIIPairList - returns pair list as a regular list of tuples
getTypeIIPairListH :: [((Char, Char), (Char, Char))] -> [((Char, Char), (Char, Char))] -> [(Char, Char)]
getTypeIIPairListH pairList constPairList = case pairList of 
   [] -> []
   (x:xs) -> if typeIIMatchInList constPairList x
      then getTypeIIMatchInList constPairList x
      else getTypeIIPairListH xs constPairList 

getTypeIIPairList :: [((Char, Char), (Char, Char))] -> [(Char, Char)]
getTypeIIPairList pairList = getTypeIIPairListH pairList pairList

--existsTypeII - True if there is a Type II move in tripCode
existsTypeII :: [(Char, Char)] -> Bool
existsTypeII tripCode = if isTypeIIPairList (getTypeIIPairs tripCode)
   then True
   else False 

--unLoop - removes last tuple from tripCode
unLoop :: [(Char, Char)] -> [(Char, Char)]
unLoop tripCode
   | odd (length tripCode) = take ((length tripCode) - 1) tripCode
   | even (length tripCode) = tripCode

--unKnot - main function
unKnotH :: [(Char, Char)] -> String
unKnotH tripCode
   | null tripCode = "unknot"
   | existsTypeI tripCode = unKnotH (removeTuples tripCode (getTypeIPair tripCode) )
   | existsTypeII tripCode = unKnotH (removeTuples tripCode ( getTypeIIPairList (getTypeIIPairs tripCode) ) )
   | otherwise = "tangle - resulting trip code: " ++ (show (unLoop tripCode))

unKnot :: [(Char, Char)] -> String
unKnot tripCode = unKnotH (loop tripCode)