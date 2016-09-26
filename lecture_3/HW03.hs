module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state name value = state_new
    where state_new :: String -> Int
          state_new x
              | x == name = value
              | otherwise = (state x)

empty :: State
empty name = 0

-- Exercise 2 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt x
    | x == True = 1
    | otherwise = 0

evalE :: State -> Expression -> Int
evalE state (Var name) = state name
evalE state (Val value) = value
evalE state (Op val1 op val2) =
    let x = (evalE state val1)
        y = (evalE state val2)
     in case op of
          Plus -> x + y
          Minus -> x - y
          Times -> x * y
          Divide -> x `div` y
          Gt -> boolToInt (x > y)
          Ge -> boolToInt (x >= y)
          Lt -> boolToInt (x < y)
          Le -> boolToInt (x <= y)
          Eql -> boolToInt (x == y)

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign str expr) = (DAssign str expr)
desugar (Incr str) = (DAssign str (Op (Var str) Plus (Val 1)))
desugar (If expr stmt1 stmt2) = (DIf expr (desugar stmt1) (desugar stmt2))
desugar (While expr stmt) = (DWhile expr (desugar stmt))
desugar (For init loop update body) = (DSequence dinit (DWhile loop (DSequence dbody dupdate)))
    where dinit = desugar init
          dbody = desugar body
          dupdate = desugar update
desugar (Sequence stmt1 stmt2) = (DSequence (desugar stmt1) (desugar stmt2))
desugar (Skip) = (DSkip)

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign name value) = (extend state name (evalE state value))
evalSimple state (DIf cond stmt1 stmt2)
  | (evalE state cond) == 1 = evalSimple state stmt1
  | otherwise = evalSimple state stmt2
evalSimple state (DWhile cond stmt)
  | (evalE state cond) == 1 = evalSimple (evalSimple state stmt) (DWhile cond stmt)
  | otherwise = state
evalSimple state (DSequence stmt1 stmt2) = evalSimple (evalSimple state stmt1) stmt2
evalSimple state (DSkip) = state

run :: State -> Statement -> State
run state stmt = evalSimple state (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
