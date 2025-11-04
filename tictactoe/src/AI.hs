module AI (firstAction,
		   minimax,
		   minimax') where 

import Data.Maybe(isJust, fromJust, isNothing)
import Data.Tuple(swap)

import Logic 


data GameTree = Node Board [(Path,GameTree)]
		deriving (Show, Eq)


data Path = Nil | NodePath { action :: Maybe Action ,
				   currentState :: Board,
				   nextNode :: Path
					} deriving (Show, Eq)

{-
lazy buildTree 
-}

buildTree :: Board -> Path -> GameTree 
buildTree board currentPath = Node board [(newPath, buildTree b' newPath) | action <- actions board, Just b' <- [result board action], let newPath = buildPath currentPath action]
			
buildPath :: Path -> Action -> Path
buildPath path act = NodePath (Just act) newState path
  where
    oldState = case path of
                 Nil -> initial_state
                 NodePath _ s _ -> s
    newState = if isNothing (result oldState act) then oldState else fromJust (result oldState act)


firstAction :: Path -> Maybe Action
firstAction path = action path


minimax' :: Board -> (Int, Maybe Action)
minimax' board = (fst found, firstAction $ snd found)
	where 
		rootPath = NodePath Nothing board Nil
		gameTree = buildTree board rootPath
		found = minimax gameTree rootPath

minimax :: GameTree -> Path -> (Int, Path)
minimax (Node board []) path = (utility board, path)
minimax (Node board children) path | terminal board = (utility board, NodePath Nothing board Nil)
							  | player board == X = maximumBy fstWithPath childResults
							  | player board == O = minimumBy fstWithPath childResults
	where 
		childResults = 
			[let (score, childPath) = minimax subTree (buildPath path ( fromJust $ action pat))
			 in (score, NodePath (action pat) board childPath)
			| (pat, subTree) <- children]


fstWithPath :: (Int, Path) -> (Int, Path) -> Ordering
fstWithPath (x, _) (y, _) = compare x y

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy cmp = foldl1 (\x y -> if cmp x y == GT then x else y)

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy cmp = foldl1 (\x y -> if cmp x y == LT then x else y)


example :: Board 
example = [[X,X,O],
		   [X,Empty,O],
		   [O,Empty,Empty]]



