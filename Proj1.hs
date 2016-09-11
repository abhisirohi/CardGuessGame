{--
Author : Abhishek Sirohi
Student ID: 727644
--}


{-
This program is the submission for the first project for the subject COMP90048
The basic idea behind the program is as follows:
Two players play this game in which one of them tries to guess the cards in the
other player's hands. Both players would draw same number of cards from the two
separate decks they have. The cards which have to be guessed are called as the
answer. The guesser would show his cards to the answerer, the answerer would then
give a hint to the guesser based on a few criterions. The guesser would then make
the nextGuess and the same process be continued until the guesser guesses the
answer correctly.
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import qualified Data.Set as SS
import Card
import Data.List
import Data.Ord (comparing)

type GameState = [[Card]]

{-
This function creates a deck of cards (52 cards)
-}
deck :: [Card]
deck = [Card s r | s <- [Club .. Spade], r <- [R2 .. Ace]]


{-
This function make all the possible combinations of n cards using the card List, for
example if we use -> combo 2 deck, we'll get a list of list of cards ->
[[Card Club R2,Card Club R3],[Card Club R2,Card Club R4]...[Card Spade Queen,Card Spade Ace],[Card Spade King,Card,Spade Ace]]
-}
combo :: Int -> [Card] -> [[Card]]
combo 0 _ = [[]]
combo n [] = []
combo n cardList = (map ((head cardList) :) (combo (n-1) (tail cardList))) ++ (combo n (tail cardList))


{-
This function calculates the feedback (hint) on the following criteria:
1. How many cards in the answer are present in the guess.
2. How many cards in the answer have lower rank than the lowest ranked card in the guess.
3. How many cards in the answer have same rank as a card in the guess.
4. How many cards in the answer have higher rank than the highest ranked card in the guess.
5. How many cards in the answer have the same suit as a card in the guess
-}
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback answer guess = (sameCard,lowRank,sameRank,highRank,correctSuit)
 where
   sameCard = howManySameCards answer guess
   lowRank = howManyLR answer guess
   sameRank = howManySR answer guess
   highRank = howManyHR answer guess
   correctSuit = howManyCS answer guess


{-
This function calculates the initital guess which would be fed to a different
function (along with the feedback obtained with the answer and initial guess)
to calculate the next guess. These are calculate upon the recommended formula
that the cards should be from different suits and should be 13/(n+1) ranks apart
(where n is the number of cards in the answer).
-}
initialGuess :: Int -> ([Card],GameState)
initialGuess n
 | n == 2   = ([Card Club R6,Card Spade R10], newGameState)
 | n == 3   = ([Card Club R5,Card Diamond Ace,Card Spade Jack], newGameState)
 | n == 4   = ([Card Club R4,Card Diamond R7,Card Heart R9,Card Spade Queen], newGameState)

 where
   newGameState = createGameState n


{-
This function creates a game state (a list of lists of every possible n cards)
for example:
createGameState 2 = [[2C,3C],[2C,4C],[2C,5C]...[QS,KS],[QS,AS],[KS,AS]]
createGameState 3 = [[2C,3C,4C],[2C,3C,5C],[2C,3C,6C]...[JS,QS,AS],[JS,KS,AS],[QS,KS,AS]]
-}
createGameState :: Int -> GameState
createGameState 0 = [[]]
createGameState n = (combo n deck)


{-
This function checks the number of cards in the answer are present in the guess.
for example:
howManySameCards [Card Club R4,Card Club R5] [Card Club R4,Card Spade R5]  == 1
howManySameCards [Card Club R4,Card Club R5] [Card Spade R5,Card Heart R2] == 0
howManySameCards [Card Club R4,Card Club R5] [Card Club R5,Card Club R4]   == 2
-}
howManySameCards :: [Card] -> [Card] -> Int
howManySameCards answer guess = length (intersectBy (\x y -> x == y) answer guess)


{-
This function checks the number of cards in the answer that have lower ranks than the lowest ranked card in the guess.
for example:
howManyLR [Card Club R4,Card Club R5] [Card Club R5,Card Spade R6] == 1
howManyLR [Card Club R4,Card Club R5] [Card Club R4,Card Spade R6] == 0
howManyLR [Card Club R4,Card Club R5] [Card Club R6,Card Spade R7] == 2
-}
howManyLR :: [Card] -> [Card] -> Int
howManyLR answer guess = SS.size (SS.filter (< minRank guess) (SS.fromList(cardToRank answer)))


{-
This function checks the number of cards in the answer that have same rank as a card in the guess.
for example:
howManySR [Card Club R4,Card Club R5] [Card Club R5,Card Spade R6] == 1
howManySR [Card Club R4,Card Club R5] [Card Club R3,Card Spade R6] == 0
howManySR [Card Club R4,Card Club R5] [Card Club R4,Card Spade R5] == 2
-}
howManySR :: [Card] -> [Card] -> Int
howManySR answer guess = SS.size (SS.intersection (SS.fromList(cardToRank answer)) (SS.fromList(cardToRank guess)))


{-
This function checks the number of cards in the answer that have higher ranks than the highest ranked card in the guess.
for example:
howManyHR [Card Club Ace,Card Club R5] [Card Club R4,Card Spade R5]    == 1
howManyHR [Card Club R4,Card Club R5] [Card Club R4,Card Spade R5]     == 0
howManyHR [Card Club Queen,Card Club Ace] [Card Club R4,Card Spade R5] == 2
-}
howManyHR :: [Card] -> [Card] -> Int
howManyHR answer guess = SS.size (SS.filter ( > maxRank guess) (SS.fromList(cardToRank answer)))


{-
This function checks the number of cards in the answer having the same suit as a card in the guess.
for example:
howManyCS [Card Club R4,Card Club R5] [Card Club R4,Card Spade R5]  = 1
howManyCS [Card Club R4,Card Club R5] [Card Club R4,Card Club R5]   = 1
howManyCS [Card Club R4,Card Club R5] [Card Heart R4,Card Spade R5] = 0
howManyCS [Card Club R4,Card Spade R5] [Card Club R4,Card Spade R5] = 2
-}
howManyCS :: [Card] -> [Card] -> Int
howManyCS answer guess = SS.size (SS.intersection (SS.fromList(cardToSuit answer)) (SS.fromList(cardToSuit guess)))


{-
This function finds the minimum rank of a list of cards
for example:
minRank [Card Club R4,Card Spade R5] = 4
-}
minRank :: [Card] -> Rank
minRank (x:xs) = minimum (cardToRank (x:xs))


{-
This function finds the maximum rank of a list of cards
for example:
maxRank [Card Club R4,Card Spade R5] = 5
-}
maxRank :: [Card] -> Rank
maxRank (x:xs) = maximum (cardToRank (x:xs))


{-
This function takes a list of cards and returns the corresponding rank (list) of each card in the list.
for example:
cardToRank [Card Club R4, Card Spade R5] = [4,5]
-}
cardToRank :: [Card] -> [Rank]
cardToRank [x] = [rank x]
cardToRank (x:xs) = map rank (x:xs)


{-
This function takes a list of cards and returns the corresponding suit (list) of each card in the list.
for example:
cardToSuit [Card Club R4, Card Spade R5] = [C,S]
-}
cardToSuit :: [Card] -> [Suit]
cardToSuit [x] = [suit x]
cardToSuit (x:xs) = map suit (x:xs)


{-
This function takes the previous guess and the previous GameState and tries to
eliminate the inconsistent combination of cards from the GameState. The next
card is chosen through a function guessCard and the gameState is the previous
GameState minus the freshly chosen card.
-}
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nexGuess (card,[[x]]) fb = (x,[[x]])
nextGuess (guess,gameState) (sameCards,lowRank,sameRank,highRank,sameSuits) = (chooseNG, nGameState)
    where
      low = minRank guess
      high = maxRank guess
      x = auxFunctGS gameState guess sameCards
      y = filter (suitCompare sameSuits guess) x
      z = filter (lowRankCompare low lowRank) y
      k = filter (highRankCompare high highRank) z
      l = filter (sameRankCompare sameRank guess) k

      chooseNG = guessCard l l
      nGameState = delete chooseNG l


{-
This function checks if the number of suits that are same in both the card lists
is equal to the 'sameSuits'
for example:
suitCompare 1 [Card Club R2,Card Club R4] [Card Club R3,Card Heart R5]  = True
suitCompare 1 [Card Club R2,Card Club R4] [Card Spade R3,Card Heart R5] = False
-}
suitCompare :: Int -> [Card] ->[Card] -> Bool
suitCompare sameSuits cardList1 cardList2
 | howManyCS cardList1 cardList2 == sameSuits = True
 | otherwise = False


{-
This function checks if the number of cards in the answer that are present in the
guess is equal to the 'sameCards'
for example:
sameCardCompare 1 [Card Club R2,Card Club R4] [Card Club R2,Card Heart R5]  = True
sameCardCompare 1 [Card Club R2,Card Club R4] [Card Spade R3,Card Heart R5] = False
-}
sameCardCompare :: Int -> [Card] -> [Card] -> Bool
sameCardCompare sameCards xs ys
 | howManySameCards xs ys == sameCards = True
 | otherwise = False


{-
This function checks if the number of cards in the answer that have same rank
as a card in the guess is equal to the 'sameRank'
for example:
sameRankCompare 1 [Card Club R2,Card Club R4] [Card Club R2,Card Heart R5]  = True
sameRankCompare 1 [Card Club R2,Card Club R4] [Card Spade R3,Card Heart R5] = False
-}
sameRankCompare :: Int -> [Card] -> [Card] -> Bool
sameRankCompare sameRank xs ys
 | howManySR xs ys == sameRank = True
 | otherwise = False


{-
This function checks if the number of cards in the answer that have rank lower than
'r' is equal to 'lowerRanks' or not.
for example:
lowRankCompare R6 2 [Card Club R2,Card Club R4]   = True
**(Read it as there are 2 cards in the list of cards that have lower rank than R6)**
lowRankCompare R2 1 [Card Spade R3,Card Heart R5] = False
-}
lowRankCompare :: Rank -> Int -> [Card] -> Bool
lowRankCompare r lowRankCards xs
 | lowRankCards == (howManyLR' r xs) = True
 | otherwise =  False


{-
This function checks if the number of cards in the answer that have rank higher than
'r' is equal to 'higherC' or not.
for example:
highRankCompare R2 2 [Card Club R3,Card Club R4]   = True
**(Read it as there are 2 cards in the list of cards that have hhigher rank than R2)**
highRankCompare R2 1 [Card Spade R3,Card Heart R5] = False
-}
highRankCompare :: Rank -> Int -> [Card] -> Bool
highRankCompare r highRankCards xs
 | highRankCards == (howManyHR' r xs) = True
 | otherwise = False


{-
This function calculates the number of cards in the card list that have lower
rank than 'r'.
for example:
howManyLR' R6 [Card Spade R3,Card Heart R5] = 2
-}
howManyLR' :: Rank -> [Card] -> Int
howManyLR' r xs = SS.size (SS.filter (<r) (SS.fromList(cardToRank xs)))


{-
This function calculates the number of cards in the card list that have lower
rank than 'r'.
for example:
howManyLR' R6 [Card Spade R3,Card Heart R5] = 2
-}
howManyHR' :: Rank -> [Card] -> Int
howManyHR' r xs = SS.size (SS.filter (>r) (SS.fromList(cardToRank xs)))


{-
This function filter out all the card combinations from the GameState which don't
have the 't' same cards.
-}
auxFunctGS :: GameState -> [Card] -> Int -> GameState
auxFunctGS _ [] _ = [[]]
auxFunctGS gameState cards t
 | t==0 = filter (sameCardCompare 0 cards) gameState
 | t==1 = filter (sameCardCompare 1 cards) gameState
 | t==2 = filter (sameCardCompare 2 cards) gameState
 | t==3 = filter (sameCardCompare 3 cards) gameState
 | t==4 = filter (sameCardCompare 4 cards) gameState
 | otherwise = filter (sameCardCompare t cards) gameState

groupFeedback :: GameState -> [Card] -> [(Int,Int,Int,Int,Int)]
groupFeedback gameState guess = map (feedback guess) gameState

{-
This function calculates the frequency of each tuple and appends the number of
occurences as the first element of the new tuple.
-}

frequency :: [(Int,Int,Int,Int,Int)] -> [(Int,(Int,Int,Int,Int,Int))]
frequency list = map (\x -> ((length x), head x)) (group list)

{-
This function calculates the weightedSum of the groups by the formula:
WS = Sum of the squares of the group sizes divided by the sum of the group sizes.
-}

weightedSum :: [(Int,Int,Int,Int,Int)] -> Double
weightedSum lst = finSum
 where
   list = map fst (frequency (lst))
   countLS1 = addList (squareOfList (list))
   countLS2 = addList (list)
   finSum = checkDiv countLS1 countLS2

{-
This function guesses the next card based on the calculation described below:
Number of occurences of every distinct feedback are calculated and then they
are squared and divided by the total number of all the occurences.
for example:
guessCard [[Card Club R3, Card Spade R4],[Card Club Ace, Card Heart King]] [[Card Club R5,Card Spade R8]] = [5C,8S]
-}
guessCard :: GameState -> GameState -> [Card]
guessCard gs cards = fst (minimumBy (comparing snd) (finSum))
 where
  finSum = zip cards (map weightedSum (map (groupFeedback gs) cards))


{-
This function squares every element in the list
for example:
squareOfList [1,2,3] = [1,4,9]
-}

squareOfList :: [Int] -> [Int]
squareOfList [] = []
squareOfList (x:xs) = x^2 : squareOfList (xs)


{-
This function adds all the numbers in the list
for example:
addList [1,2,3] = 6
-}
addList :: [Int] -> Int
addList [] = 0
addList (x:xs) = x + addList (xs)


{-
This function divides the two integer numbers and gives the result in double
-}
checkDiv :: Int -> Int -> Double
checkDiv x y = a / b
  where a = fromIntegral x :: Double
        b = fromIntegral y :: Double
