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
extend st s v = \ss -> if ss == s then v else st ss

empty :: State
empty = \_ -> 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE st (Val i) = i
evalE st (Op e1 op e2)
  | op == Plus     = (evalE st e1) + (evalE st e2)
  | op == Minus    = (evalE st e1) - (evalE st e2)
  | op == Times    = (evalE st e1) * (evalE st e2)
  | op == Divide   = (evalE st e1) `div` (evalE st e2)
  | op == Gt       = if (evalE st e1) > (evalE st e2) then 1 else 0
  | op == Ge       = if (evalE st e1) >= (evalE st e2) then 1 else 0
  | op == Lt       = if (evalE st e1) < (evalE st e2) then 1 else 0
  | op == Le       = if (evalE st e1) <= (evalE st e2) then 1 else 0
  | op == Eql      = if (evalE st e1) == (evalE st e2) then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign st ex) = DAssign st ex
desugar (Incr st) = DAssign st (Op (Var st) Plus (Val 1))
desugar (If ex st1 st2) = DIf ex (desugar st1) (desugar st2)
desugar (While ex st) = DWhile ex (desugar st)
desugar (For initst ex updst bodyst) = 
 DSequence (desugar initst) ( DWhile ex (DSequence (desugar bodyst) (desugar updst)))
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar Skip = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s ex) = extend st s (evalE st ex)
evalSimple st DSkip = st
evalSimple st (DIf ex ds1 ds2) = 
    let b = evalE st ex
    in if b /= 0 then evalSimple st ds1 else evalSimple st ds2
evalSimple st (DWhile ex ds) =
    let b = evalE st ex
    in if b /= 0
       then let st' = evalSimple st ds
            in evalSimple st' (DWhile ex ds)
       else st
evalSimple st (DSequence ds1 ds2) = 
    let st' = evalSimple st ds1
    in evalSimple st' ds2

run :: State -> Statement -> State
run st stmnt = evalSimple st (desugar stmnt)

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
