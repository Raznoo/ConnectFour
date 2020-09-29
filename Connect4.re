open SigGame
open CS17SetupGame

module Connect4 = {
  /*
  Data Type:
    Matrix: is a nonempty list of non empty lists of numbers where
      each row of the matrix is represented by a list of length n
      a matrix can either be a square matrix or a rectangular matrix
    Square Matrix: is a matrix where the length of each row (n) is
      equivalent to the number of rows (k)
    Rectangular Matrix: is a matrix where the length of each row (n)
      is not equivalent to the number of rows(k)

    WhichPlayer is either...
      |P1
      |P2

    status is either...
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);

    state is a tuple containing a (list(list(int)), status) tuple where
      the list(list(int)) is the current game board and status is the current
      status of the game
      
  Example data:
    Matrix: [[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[1, 2, 3]], [[1], [2], [3]]
    Square Matrix:  [[1]], [[1, 2], [3, 4]],
                    [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    Rectangular Matrix: [[1, 2]], [[1, 2, 3], [4, 5, 6]], [[1], [2], [3]]
    WhichPlayer: P1, P2
    status: Win(P1), Draw, Ongoing(P2)
    state([[0, 0, 0, 0],
          [0, 0, 0, 0],
          [0, 0, 0, 0],
          [0, 0, 0, 0]], Ongoing(P1))

*/
  let globalRow= 8
  let globalCol= 7

  type matrix('a) = list(list(int));
/*
  Type signature:
    transpose: Matrix => Matrix

  Input:
    mtrx, a matrix

  Output: the transpose of the input matrix

  OI: [[1, 2, 3], [1, 2, 3], [1, 2, 3]]
  RI: [[1, 2, 3], [1, 2, 3]]
  RO: [[1, 1], [2, 2], [3, 3]]
  Ideation: Add another element to each of the lists in the recursive output
  corresponding to the first second and third element in each of the lists
  in the original input
  OO: [[1, 1, 1], [2, 2, 2], [3, 3, 3]]
*/
  let rec transpose: matrix('a) => matrix('a) =
  mtrx =>
    switch (mtrx) {
    | [] => []
    | [[], ..._] => failwith("A matrix cannot be 0-dimensional.")
    | [[hd], ..._] => [List.flatten(mtrx)]
    | [[hd, ...tl], ..._] =>
        [List.map(List.hd, mtrx),...
        transpose(List.map(List.tl, mtrx))]
    };
/*
    Type signature:
      horzFlip: Matrix => Matrix

    Input:
      mtrx, a matrix

      Output: the horizontal flip of the input matrix   
*/              
let horzFlip: matrix('a) => matrix('a) = mtrx => List.rev(mtrx);
/*
    Type signature:
      vertFlip: Matrix => Matrix

    Input:
      mtrx, a matrix

      Output: the vertical flip of the input matrix   
*/              
let vertFlip: matrix('a) => matrix('a) = mtrx => List.map(List.rev, mtrx);
/*
    Type signature:
      recTpose: Matrix => Matrix

    Input:
      mtrx, a matrix

      Output: the reverse transpose of the input matrix   
*/              
let revTpose: matrix('a) => matrix('a) = mtrx =>
vertFlip(transpose(horzFlip(mtrx)));

  /* player 1 is P1, player 2 is P2 */
  type whichPlayer =
    | P1
    | P2;

  /* either a player has won, it's a draw, or it's ongoing */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);
    
  type state= State(list(list(int)),  status)

  /* describes a move that a player can make */
  /*int represents the row that the player is making their move on */
    type move = Move(int)

/*
  type signature
  stringOfPlayer: whichPlayer => string

  Input:
  player, which player is being represented

  Output:
  a string representation of the player
  */
  let stringOfPlayer: whichPlayer => string = player =>
  switch(player){
    | P1 => "Player 1"
    | P2 => "Player 2"
  };

/*  prints out the current state of the game */
/*
  type signature:
  stringOfState: state => string

  Input:
  stat, the current state of the board

  Output:
  the string representation of the real (reverse transposed) game board
*/
    
  let stringOfState: state => string = stat =>{
      let listToString: matrix('a) => string = mtrx =>{
        let rec listToStringHelper: list(int) => string = list =>
        "|" ++ String.concat(" ", (List.map(string_of_int, list))) ++ "|"
      String.concat("\n", (List.map(listToStringHelper, mtrx)))}
    switch(stat){
      | State(grid,Ongoing(p)) => listToString(revTpose(grid)) ++ 
        " waiting on " ++ stringOfPlayer(p)
      | State(_, Draw)=> "it is a draw"
      | State(_, Win(p)) => stringOfPlayer(p) ++ " has won"
      }
      };
      
/*
  type signature:
  stringOfMove: move => string

  Input:
  mov, the move being taken

  Output:
  a string representation of the move
*/
    let stringOfMove: move => string = mov =>
    switch(mov){
      | Move(x) => "Placing checker in row " ++ string_of_int(x) 
    };
    
/*will take in an int and make a list of zeros with length equal to the
input length*/
/*
  type signature:
  rowConstructor: int => list(int)

  Input:
  columns, an integer

  Output:
  a list of zeros with a list length equal to the integer input

  OI: 4
  RI: 3
  RO: [0, 0, 0]
  Ideation: [add another zero to the front]
  OO: [0, 0, 0, 0]
*/ 
  let rec rowConstructor: int => list(int) = columns =>
    switch(columns){
    | 0 => failwith("cannot have zero columns")
    | 1 => [0]
    | x => [0, ... rowConstructor(x-1)]
    };

/* will take in an (int, int) tuple that represents the number of rows and
  columns respectively*/
/*
  type signature:
  gridConstructor: int * int => list(list(int))

  Input:
  rows, an integer
  columns, an integer

  Output:
  list of list of zeros (matrix) where columns represent list length of each
  sublist and rows represents the list length of the list of lists of integers
*/
  let rec gridConstructor: (int, int) => matrix('a) = (rows, columns) =>
    switch(rows){
      | 0 => failwith("cannot have zero rows")
      | 1 => [rowConstructor(columns)]
      | x => [rowConstructor(columns), ... gridConstructor(x-1, columns)]
    };
/* initial state is an arbitrarily sized grid that can be altered by changing
the globalRow or globalColumn variables
initial state will always begin with player 1
*/
  let initialState = 
    State(gridConstructor(globalCol, globalRow), Ongoing(P1))
    
/* produces the list of legal moves at a state */
/*
  type Signature:
  legalMoves: state => list(move)

  Input:
  stat, a state

  Output:
  a list of all legal moves that can be made from the input state
*/
    let legalMoves: state => list(move) = stat =>{
      let rec legalMovesHelper: (int, matrix('a)) => list(move)= 
      (currentRow, grid) =>
        switch(currentRow, grid){
          | (_, [[]]) => []
          | (x, [[0, ...tl]]) => [Move(x)]
          | (x, [[num, ...tl]]) when (num != 0) => []
          | (x, [[num, ...tl], ...tl2]) when (num != 0)=>
            legalMovesHelper(currentRow + 1, tl2)
          | (x, [[0, ...tl], ...tl2]) => [Move(x), ...
            legalMovesHelper(currentRow + 1, tl2)]
          | (_, _) => failwith("improper input")
        }; 
    switch(stat){
      | State(grid, Ongoing(_)) => legalMovesHelper(1, grid)
      | State(grid, _) => failwith("no legal moves")
    }
    };

/* returns the status of the game at the given state */
/*
  type signature:
  gameStatus: state => status

  Input:
  (State(_, stat)), where stat is the current status of the game

  Output:
  the current status of the game
*/
    let gameStatus: state => status = (State(_, stat)) => stat

/* helpers for nextState*/
/*
  type signature:
  addXToRow: (int * int * matrix('a)) => matrix('a)

  Input:
  x, an integer representing which "checker" being added (1 for P1, 2 for P2)
  row, an integer representing the row being added to
  grid, the grid being manipulated

  Output:
  a new matrix representing checker (x) added to row (row) in the appropriate
  place in (grid)
*/
    let addXToRow: (int, int, matrix('a)) => matrix('a) = (x, row, grid) =>{
      let rec addXToRowHelper:(int,int,matrix('a))=>matrix('a)=(x,row,grid)=>{
        let rec addX: (int, list(int)) => list(int) = (x, row) =>
        switch(x, row){
          | (x, []) => failwith("illegal move")
          | (x, [hd, ... tl]) when (hd != 0) => [hd, ...addX(x, tl)]
          | (x, [hd, ... tl]) when (hd == 0) => [x, ...tl]
          };
      switch(row, grid){
        | (1 , [row, ...tl])=> [addX(x, row), ...tl]
        | (1 , _) => failwith("row doesn't exist")
        | (n , [row, ...tl])=> [row, ...addXToRowHelper(x, n-1, tl)]
      };
      };
    vertFlip(addXToRowHelper(x, row, vertFlip(grid)));
    };

/*
  type signature:
  diagonalConstructor: matrix('a) => list(list(int))

  Input:
  grid, a matrix('a) representing the grid being manipulated

  Output:
  a list(int) that represents all of the possible right diagonals that can be
  made from the input grid

  OI: [[1, 2, 3],
       [4, 5, 6]]
  RI: [[4, 5, 6]]
  RO: [[4], [5], [6]]
  Ideation: cons the first element of the OI onto the front of the RO, then
  the second onto the 1st element, then the 3rd onto the 2nd elemnt....
  OO: [[1], [2, 4], [3, 5], [6]]
*/
  let rec diagonalConstructor: matrix('a) => list(list(int)) = grid =>{
    let rec diagConstructHelp: (list(int), list(list(int))) => 
      list(list(int)) = (addRow, otherRows) =>
        switch (addRow, otherRows) {
          | ([], [hd, ...tl]) => otherRows
          | ([hd, ...tl], []) => List.map(x => [x], addRow)
          | ([hd1, ...tl1], [hd2, ...tl2]) => 
            [[hd1, ...hd2], ...diagConstructHelp(tl1, tl2)]
        };
      switch (grid) {
        | [] => []
        | [[hd, ...tl1], ...tl] => [[hd], ...
          diagConstructHelp(tl1, diagonalConstructor(tl))]
      };
   };
  
/* given a state and a legal move, yields the next state */
/*
  type signature:
  nextState: (state * move) => state

  Input:
  stat, a state
  moveArg, a move to be applied to stat

  Output:
  the next state that happens as a result of applying moveArg to stat
*/
  let nextState: (state, move) => state = (stat, moveArg) => {
    let winCheck: (matrix('a),move, int) => bool= (grid, moveArg, player) =>{
      let rec gridFold: (matrix('a),int, int) => bool ={
          let rec rowCheck: (int, int, int, list(int)) => bool = 
          (player,counter, constant, aloi) =>
          switch(counter, aloi){
          | (_, []) => false
          | (1, [hd, ...tl]) when (hd == player) => true
          | (x, [hd, ...tl]) when (hd == player) => 
            rowCheck(player, x-1, constant, tl)
          | (x, [hd, ...tl]) => 
            rowCheck(player, constant, constant, tl)
    };
      (grid, inARow, playNum) =>
      switch(grid){
        | [] => false
        | [hd, ...tl] => rowCheck(playNum, inARow,inARow, hd) || 
          gridFold(tl, inARow, playNum)
      }
      };
    switch(grid, moveArg){
      |(grid, Move(mov)) => 
      gridFold(addXToRow(player, mov, grid), 4, player) ||
      gridFold(transpose(addXToRow(player, mov, grid)), 4, player) || 
      gridFold(
        diagonalConstructor(addXToRow(player, mov, grid)), 4, player) ||
      gridFold(
        diagonalConstructor(vertFlip(addXToRow(player, mov, grid))),4, player)
    }
    };
    switch(stat, moveArg){
      | (State(grid, Ongoing(P1)), Move(x)) when 
        winCheck(grid, moveArg, 1)=> 
          State(addXToRow(1, x, grid), Win(P1))
      | (State(grid, Ongoing(P2)), Move(x)) when 
        winCheck(grid, moveArg, 2) => 
          State(addXToRow(2, x, grid), Win(P2))
      | (State(grid, _), _) when (legalMoves(stat)== []) => 
          State(grid, Draw)
      | (State(grid, Ongoing(P1)), Move(x)) =>  
          State(addXToRow(1, x, grid), Ongoing(P2))
      | (State(grid, Ongoing(P2)), Move(x)) =>
          State(addXToRow(2, x, grid), Ongoing(P1))
    }
    };

  /* for transforming human player input into
    internal representation of move */
/*
  type signature:
  moveOfString: string => move

  Input:
  str, the string representation of a move

  Output:
  the move that the input string represents
*/
  let moveOfString: string => move = str => Move(int_of_string(str))

/* estimates the value of a given state (static evaluation) */
/*
  type signature:
  estimateValue: state => float

  Input:
  State(grid, stat), a state

  Output:
  the value of the input state represented as a float
*/
    let estimateValue: state => float = (State(grid, stat)) => {
      let rec rowValues: list(list(int)) => float = grid =>
        switch(grid){
        | [] => 0.0
        | [hd, ...tl] => {
          let rec rowValuesHelp: list(int) => float = row =>
          switch (row) {
          | [one, two, three, four, ...rest] => 
              switch(one, two, three, four) {
              | (2, 2, 2, 2) => infinity  
              | (2, 0, 2, 2) 
              | (2, 2, 2, 0) 
              | (2, 2, 0, 2) 
              | (0, 2, 2, 2) =>
                  1000.0 +. rowValuesHelp([two, three, four, ...rest])                 
              | (2, 0, 2, 0) 
              | (2, 2, 0, 0) 
              | (0, 2, 0, 2)  
              | (2, 0, 0, 2) 
              | (0, 2, 2, 0) 
              | (0, 0, 2, 2) => 
                  100.0 +. rowValuesHelp([two, three, four, ...rest])
              | (2, 0, 0, 0) 
              | (0, 2, 0, 0) 
              | (0, 0, 2, 0) 
              | (0, 0, 0, 2) => 
                  10.0 +. rowValuesHelp([two, three, four, ...rest])    
              | (1, 1, 1, 1) => neg_infinity
              | (0, 1, 1, 1) 
              | (1, 0, 1, 1) 
              | (1, 1, 0, 1) 
              | (1, 1, 1, 0) => 
                  -1000.0 +. rowValuesHelp([two, three, four, ...rest])
              | (1, 0, 1, 0) 
              | (1, 1, 0, 0) 
              | (1, 0, 0, 1) 
              | (0, 1, 0, 1) 
              | (0, 0, 1, 1) 
              | (0, 1, 1, 0) => 
                  -100.0 +. rowValuesHelp([two, three, four, ...rest])
              | (0, 0, 1, 0)
              | (1, 0, 0, 0) 
              | (0, 1, 0, 0) 
              | (0, 0, 0, 1) => 
                  -10.0 +. rowValuesHelp([two, three, four, ...rest])  
              | (_, _, _, _) => 
                  0.0 +. rowValuesHelp([two, three, four, ...rest])
              }
         /*cases like the diagonal check that don't have 4 element  lists */
        | _ => 0.0    
        };
      rowValuesHelp(hd) +. rowValues(tl)
    }
    }
    /*adds all of the results of all rows, columns, and diagonals defined 
    previously in elseValues*/
    rowValues(grid) +. rowValues(transpose(grid)) +. 
    rowValues(diagonalConstructor(vertFlip(grid))) +. 
    rowValues(diagonalConstructor(grid))
    };
    
    };

module Game : Game = Connect4;
open Connect4;
/* test cases */
    checkExpect(
      transpose([]),
      [],
      "empty matrix"
    );
    checkError(() => 
      transpose([[], [1, 2], [1, 2]]), 
      "A matrix cannot be 0-dimensional."
    );
    checkExpect(
      transpose([[1], [2], [3]]),
      [[1, 2, 3]],
      "1 element matrix"
    );  
    checkExpect(
      transpose([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
      [[1, 1, 1], [2, 2, 2], [3, 3, 3]],
      "test 1"
    );
    checkExpect(
  horzFlip([[3, 2, 1],[6, 5, 4],[9, 8, 7]]),
  [[9, 8, 7], [6, 5, 4], [3, 2, 1]],
  "test 1"
    );
    checkExpect(
  vertFlip([[3, 2, 1],[6, 5, 4],[9, 8, 7]]),
  [[1, 2, 3], [4, 5, 6], [7, 8, 9]],
  "test 2"
    );

checkExpect(
  revTpose([[1, 1, 1], [2, 2, 2], [3, 3, 3]]),
  [[1, 2, 3], [1, 2, 3], [1, 2, 3]],
  "test 3"
);
  checkExpect(
    stringOfMove(Move(2)),
    "Placing checker in row 2",
    "row placer"
  );
    checkError(() => 
    rowConstructor(0), 
    "cannot have zero columns"
  );
  checkExpect(
    rowConstructor(1),
    [0],
    "single row rowConstructor"
  );
  checkExpect(
    rowConstructor(4),
    [0,0,0,0],
    "multiple row constructor"
  );
   checkError(() => 
    gridConstructor(0), 
    "cannot have zero rows"
  );
  checkExpect(
    gridConstructor(1, 1),
    [[0]],
    "single row gridConstructor"
  );
  checkExpect(
    gridConstructor(4, 4),
    [[0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]],
    "multiple row constructor"
  );

  checkExpect(
    stringOfPlayer(P1),
    "Player 1",
    "player 1"
  );
  checkExpect(
    stringOfPlayer(P2),
    "Player 2",
    "player 2"
  );
    checkExpect(
    stringOfState(State([[1, 0, 1, 0], [0, 1, 0, 1], [1, 0, 1, 0], 
      [0, 1, 0, 1]], Ongoing(P1))),
    "|1 0 1 0|\n|0 1 0 1|\n|1 0 1 0|\n|0 1 0 1| waiting on Player 1",
    "ongoing game"
  );
  checkExpect(
    stringOfState(State([[1, 0], [0, 1], [1, 0], [0, 1]], Draw)),
    "it is a draw",
    "a draw"
  );
  checkExpect(
    stringOfState(State([[1, 0], [1, 0], [1, 0], [1, 0]], Win(P1))),
    "Player 1 has won",
    "game won"
  ); 

    checkExpect(
      addXToRow(1, 1, [[0, 0, 0, 0], [0, 0, 0, 0]]),
      [[0, 0, 0, 1], [0, 0, 0, 0]],
      "addXToRow check expect"
    );

checkError(()=>
    addXToRow(1, 1, [[]]),
    "illegal move"
);

checkError(()=>
  addXToRow(1, 2, [[0, 0, 0, 0]]),
  "row doesn't exist"
);
checkExpect(
  addXToRow(1, 2, [[0, 0, 0, 0],[0, 0, 0, 0]]),
  [[0, 0, 0, 0],[0, 0, 0, 1]]
);
checkExpect(
  addXToRow(1, 1, [[0, 0, 0, 1], [0, 0, 0, 0]]),
  [[0, 0, 1, 1], [0, 0, 0, 0]],
  "addXToRow check expect 2"
);

   checkExpect(
    diagonalConstructor([[1, 2, 3],[4, 5, 6]]),
    [[1], [2, 4], [3, 5], [6]],
    "diagonalConstructor list test"
  ); 
  checkExpect(
    diagonalConstructor([]),
    [],
    "diagonalConstructor empty test"
  );  
checkExpect(
  diagonalConstructor([[1, 2, 3]]),
  [[1], [2], [3]],
  "single row check"
);

checkExpect(
  moveOfString("1"),
Move(1),
"move 1 test"
  );