instructions for use, describing how a user would interact with your program (how would
someone play your game against a friend? against the AI?)
_______________________________________________________________________________________

---------------------------------------------------------------------------------------
an overview of how your program functions, including how all of the pieces fit together
_______________________________________________________________________________________
GENERAL OVERVIEW
The connect four game is an ongoing loop that only ends when some fail case is met or
there is a definite result of the game (either a player wins or there are no legal
moves in the current state). The game will begin with player 1, then wait for him to 
make a move. After the player makes a move, the state is passed to the referee, then 
that new state is passed to player 2. Player 2 will then chose a legal move for the
state, then that new state will be passed to he referee.

player 1 and player 2 can either both be AI, both be human, or one of each
///////////////////////////////////////////////////////////////////////////////////////
ConnectFour MODULE
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
MATRICES

the module begins by defining the "grid" that we will be working with throughout the
module as a matrix('a) which is defined as a list(list(int)). This is mostly done to
separate any operation that inputs or outputs a list(list(int)) that is not the grid
such as the diagonal constructor that we will talk about later.

also in this module are several matrix manipulation operations that are used to
expedite some of the operations to manipulate the grid
	horzFlip
	vertFlip
	transpose
	revTpose(which undoes what transpose does)
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
GAME SETUP 

in this section we set up perameters for the game to operate in such as...

-the global variables (globalRows, globalColumns) that are used to alter the size of
the grid the game is being played on
-player which states that player1 is P1 and player 2 is P2
-status which is every possible status of the game
-state which is the data type used to express the current grid and current status
-initialState and rowConstructor provides the start of the game while operating under
	the perameters that the grid must be arbitrary in size

additionally is should be noted that the state data type does use matrix('a) as the 
type for the grid. Our grids are represented in state as a list of columns where
the index of the columns in their list(plus 1) corresponds to their column in the
"real" grid. Also the number 1 represents a checker owned by player 1 and a number 2
respresents a checker owned by player 2
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
STRING OF _

this section describes each conversion of a data type to a string. This section is
important because it provides some "printable" progress of the game to be relayed to a
human that might be playing or watching.

-stringOfState: prints the current grid as well as the current player
-stringOfMove: prints the move that was just inputted
-stringOfPlayer: converts which player is being referenced to a string representation
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
THE GAME

this section is every function that actually makes each game work. NO WORK IS DONE IN
THIS SECTION. This entire section just defines functions to be called in other modules.
As an example, a list of legal moves is not actually produced in the ConnectFour 
module while player 1 is making his move, it is instead either computed in the 
AIPlayer or HumanPlayer module.

-Legal moves will input a state an output a list of every possible legal move that can 
be made in that state. A move will not be included in that list if the corresponding
row is completely "filled" IE there are no zeros in the row
-gameStatus gives the current status of the game
-addXToRow will take in an integer (x), an integer(row), and a matrix('a)(grid) and
will, through recursion and matrix manipulation, add the number (x) which represents
the "checker" being added to the (row) in (grid). After the checker is added, the
reverse of the initial matrix manipulation is done so that the function outputs a state
that is formatted correctly
- diagonalConstructor is used anytime a reference needs to be made to the diagonals
that exist in the grid. It creates a list of every possible right diagonal
	- to check the left diagonals either transpose or revTPose the grid before
	using diagonalConstructor
-nextState inputs a state and a move to be applied to that state and outputs the next
state that will occur by applying the input move to the input state. nextState will
say a player wins if the resulting state passes winCheck, a local procedure that checks
every row, column, and diagonal (both ways) and returns true if any of these 
interperetations of the matrix include 4 of the same checker in a row (another part of
this is the gridFold local procedures that does the same thing as an OR fold on all of
the rows, columns, etc that have been rowCheck'ed).
	- rowCheck is a function that inputs an integer (player), an integer (counter), an 
integer (constant), and a list(int) (aloi) and outputs a boolean. This function will 
usually take in a row of arbitrary length to check but it is also used as a check for
the output of diagonalConstructor. This function has the same number inputted twice 
because the (counter) will continue counting down as the function recurs through (aloi)
every element in (aloi) that matches (player) will reduce the (counter) by 1. If there 
is an element in the list that does not match (player) then the (counter) will reset
by referencing (constant). If the list completely recurs without the (counter) reaching
zero, the function will return false
-estimateValue inputs a matrix('a)(grid) and outputs the float "value" of that grid.
this function uses pattern matching to try and find advantageous patterns (such as 3
in a row) to provide a context for minimax for the AI to use. This is accomplished by
using the helper function rowValueHelper on the head of the input matrix and then
adding that value to the recursive call on the rest of the list (evaluate each row then
add up the value of all the rows). This function is done on the column representation
of the function, row representation, as well as the diagonalConstructor of the standard
grid and the diagonalConstructor of the transposed grid(for vertical, horizontal,
diagonal both ways respectively) then those results are added up to produce a final
value of the state.
	-player 1 having 3 in a row is bad for player 2 who will be using estimateValue
	so these cases have negative floats
	-Player 1 having 3 in a row is worse than player 2 having 2 in a row, this is
	where the ideology for blocking originates because the AI will "look into the
	future" and decide that preventing the other player from winning is more
	valuable than almost winning
///////////////////////////////////////////////////////////////////////////////////////
AIPlayer

Our minimax algorithm works through a list of all of the evaluated states by...

going through the treeKiller function. The program is named this because it makes a
"tree" (a list of floats that operates in the same logic as a tree) and filters through
it with  the minOrMax function. This function is used at every step of the 
"tree making" process because when the evaluated leaves create a list of floats at
the final depth, the minOrMax function will find the max(or min) of that list, then of
the list of the max(or min) floats, an opposite of max or min needs to occur from that
list and this process occurs until "depth" 0 is reached
The minOrMax function will return the highest value of the inputted list through
accumulation
getMove will (when it is implemented) follow treeKiller through its creation and 
destruction process and return the associated legal move that came out on the top of
the tree

the booleans are used to represent either min or max. the booleans are switched to 
represent alternating decisions as both players traverse the tree. at the end of the
AI module there's a switch statement for the player so that treeKiller can properly
traverse the tree and so the appropriate actions can be taken for estimate value if
the AI is either player 1 or player 2.
---------------------------------------------------------------------------------------
a description of any possible bugs or problems with your program
_______________________________________________________________________________________
none
---- -----------------------------------------------------------------------------------
a list of the people with whom you collaborated
_______________________________________________________________________________________
Robert Boudreaux
Ethan Royer
---------------------------------------------------------------------------------------
a description of any extra features you chose to implement
_______________________________________________________________________________________
none
---------------------------------------------------------------------------------------