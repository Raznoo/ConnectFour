open CS17SetupGame
open SigGame
open Connect4

module AIPlayer = (Connect4: Game) => {
  module PlayerGame = Connect4
  open Connect4
let nextMove: state => move = stat => {
  let rec treeKiller: (state, int, bool) => float = (stat, depth, mmBool) =>{
   let minOrMax: (list(float) , bool) => float = (deadTree, mmBool) => {
    let rec accumulator: (list(float), float, bool) => float = 
    (leaves, currentVal, mmBool) =>
    switch (leaves) {
    | [hd] => currentVal
    | [hd, ...tl] when mmBool => 
      if (hd > currentVal) 
        {accumulator(tl, hd, mmBool)} 
      else 
        {accumulator(tl, currentVal, mmBool)};
    | [hd, ...tl] => 
      if (hd < currentVal) 
        {accumulator(tl, hd, mmBool)} 
      else 
        {accumulator(tl, currentVal, mmBool)};
    };
    switch(deadTree){
      | [] => 0.0
      | [hd, ...tl] => accumulator(deadTree, hd, mmBool)
    }
  };
  switch (gameStatus(stat), depth) {
  | (Win(_), _) => estimateValue(stat)
  | (Draw(_), _) => estimateValue(stat)
  | (Ongoing(_), 0) => estimateValue(stat)
  | (Ongoing(_), x) => minOrMax((List.map(x => treeKiller(x, depth - 1, 
    !mmBool), List.map(x => nextState(stat, x), legalMoves(stat)))), mmBool) 
  };
  };
  let rec getMove: (list((float, move)), bool) => move = (fmPair, mmBool) => {
    let rec getMoveHelper: (list((float, move)), (float, move), bool) => move =
    (input, currentVal, mmBool) =>
    switch (input, currentVal){
      | ([hd], (currVal, currMv)) => currMv
      | ([(flt, mv), ...tl], (currVal, currMv)) when mmBool =>
        if (flt > currVal) 
          {getMoveHelper(tl, (flt, mv), mmBool)}
        else 
          {getMoveHelper(tl, currentVal, mmBool)};
      | ([(flt, mv), ...tl], (currVal, currMv)) => 
        if (flt < currVal) 
          {getMoveHelper(tl, (flt, mv), mmBool)}
        else 
          {getMoveHelper(tl, currentVal, mmBool)}
    }
    getMoveHelper(fmPair, List.hd(fmPair), mmBool)
  }
    switch(gameStatus(stat)){
  | Ongoing(x) => if (x == P1) 
      {getMove(List.map(x => (treeKiller(nextState(stat, x), 4, false), x), 
        legalMoves(stat)), false)}
      else
      {getMove(List.map(x => (treeKiller(nextState(stat, x), 4, true), x), 
        legalMoves(stat)), true)};
  }
   
  }
 
};

module TestGame = Connect4;
module TestAIPlayer = AIPlayer(TestGame); 
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */
