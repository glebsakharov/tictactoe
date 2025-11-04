module Logic (Piece(..),
			  Board,
			  Action,
			  initial_state,
			  coordinates,
			  numOf,
			  player,
			  actions,
			  result,
			  winner, 
			  terminal,
			  utility) where

import Data.List(elemIndex,unfoldr,transpose,reverse)

data Piece = O | X | Empty
	deriving (Show, Eq)

type Board = [[Piece]]

type Action = (Int, Int)

initial_state :: Board 
initial_state = [[Empty, Empty, Empty],
				 [Empty, Empty, Empty],
				 [Empty, Empty, Empty]]

coordinates :: [Action]
coordinates = [(0,0),(0,1),(0,2),
			   (1,0),(1,1),(1,2),
			   (2,0),(2,1),(2,2)]

numOf :: Board -> Piece -> Int 
numOf board piece = length $ filter (==piece) (concat $ board)

player :: Board -> Piece 
player board | board == initial_state = X 
			 | (numOf board Empty) `mod` 2 == 0 = O 
			 | (numOf board Empty) `mod` 2 == 1 = X 
			 | otherwise = O 

actions :: Board -> [Action] 
actions board = acts
	where 
		flattenedNZipped = zip (concat board) coordinates
		filtered = filter (\x -> fst x == Empty) flattenedNZipped
		acts = map snd filtered


result :: Board -> Action -> Maybe Board 
result board action | elem action (actions board) = Just $ chunk 3 newBoardPieces
					| otherwise = Nothing
	where 
		flattenedNZipped = zip (concat board) coordinates
		newBoardPieces = map (\(p,c) -> if c == action then player board else p) flattenedNZipped


chunk :: Int -> [a] -> [[a]]
chunk n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))


winner :: Board -> Piece 
winner board | (horizontal X || vertical X || diagonalLtR X || diagonalRtL X) = X
		     | (horizontal O || vertical O || diagonalLtR O || diagonalRtL O) = O
		     | otherwise = Empty
	where 
		horizontal plyr = any (\row -> all (==plyr) row) board
		vertical plyr = any(\row -> all (== plyr) row) (transpose board)
		diagonalLtR plyr = all (==plyr) (diag board)
		diagonalRtL plyr = all (==plyr) (diag(map reverse board))

diag :: [[a]] -> [a]
diag ((a:_):rs) = a : diag (map (drop 1) rs)
diag _          = []

example :: Board 
example = [[X,Empty,O],
		   [X,Empty,O],
		   [X,Empty,Empty]]

terminal :: Board -> Bool 
terminal board | numOf board Empty == 0 = True
		       | winner board == X || winner board == O = True 
		       | otherwise = False

utility :: Board -> Int 
utility board | winner board == X = 1 
			  | winner board == O = -1
			  | otherwise = 0






