# Tic-Tac-Toe in Haskell, with minimax AI and Gloss GUI

Play against an AI that is unbeatable at tic-tac-toe (noughts and crosses). Leveraging the minimax algorithm, the AI determines the best move at each turn, resulting in a game that always either ends in a draw, or with the AI beating the human player (you).

# How to Use

Download the folder tictactoe from this repository. If you have stack installed, cd into tictactoe in the terminal and run:

``` zsh
stack run
```
From here, you are player one, or 'X'. Pick a square and play tic-tac-toe against the AI. Can you win? No, mathematically impossible, if I have done my job right.

# Explore

The important code resides in the src directory of the tictactoe folder. There, you will find AI.hs, Logic.hs and UI.hs. 

AI.hs contains minimax and some helper functions: this is the decision making core of the program. The minimax function enables the "AI" to choose the optimal move at each turn, by searching the entire game-tree for the path that returns a utility of -1. Because the AI is O and O is the minimising player, this is the path the AI must choose, and so the first action in the path found is selected. 

Logic.hs contains some useful functions such as result, winner, actions, utility, terminal etc. These determine the resulting board from a board and an action to take, the winner of the game, the possible actions given a boardm the utility of the board and whether or not the board is a final board. They comprise the games' logic. 

UI.hs should be self explanatory. It writes the graphical user interface of the game, using the Gloss library. At the moment, it only enables you to play as X, and doesn't allow resets, which is something I will be working on.

