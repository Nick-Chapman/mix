module Flow (Value(N),gcd,run) where

import Prelude hiding (lookup,gcd)

data Prog = Prog [Var] [Block] deriving (Show)

data Block = Block Lab Code deriving (Show)

data Code
  = Assign Var Exp
  | Goto Lab
  | If Exp Lab Lab
  | Finish Exp
  deriving (Show)

data Exp
  = ECon Value
  | EVar Var
  | EOp String [Exp]
  deriving (Show)

newtype Var = Var String deriving (Eq,Show)
newtype Lab = Lab Int deriving (Num,Enum,Eq,Show)
data Value = N Int | B Bool deriving (Eq,Show,Ord)

gcd :: Prog
gcd = Prog [x,y] $ blocks
  [ If (EOp "=" [EVar x,EVar y]) 7 2
  , If (EOp "<" [EVar x,EVar y]) 5 3
  , Assign x (EOp "-" [EVar x,EVar y])
  , Goto 1
  , Assign y (EOp "-" [EVar y,EVar x])
  , Goto 1
  , Finish (EVar x)
  ]
  where
    blocks = zipWith Block [1..]
    x = Var "x"
    y = Var "y"

run :: Prog -> [Value] -> Value
run (Prog formals blocks) actuals = exec goto store 1 (goto 1)
  where
    store :: Store = foldl update store0 (zip formals actuals)
    goto :: Lab -> Code
    goto dest = head [ code | Block lab code <- blocks, lab == dest ]

exec :: (Lab -> Code) -> Store -> Lab -> Code -> Value
exec goto store loc = \case
  Assign x e -> exec goto (update store (x,eval store e)) (loc+1) (goto (loc+1))
  Goto lab -> exec goto store lab (goto lab)
  If i t e -> opIte t e (eval store i)
  Finish e -> eval store e
  where
    opIte t e = \case
      B True -> exec goto store t (goto t)
      B False -> exec goto store e (goto e)
      cond -> error $ "opIte, " <> show cond

eval :: Store -> Exp -> Value
eval store = \case
  ECon value -> value
  EVar var -> lookup store var
  EOp "=" args -> opEq (map (eval store) args)
  EOp "<" args -> opLess (map (eval store) args)
  EOp "-" args -> opMinus (map (eval store) args)
  op@EOp{} -> error $ show op
  where
    opEq = \case [v1,v2] -> B $ v1 == v2; args -> error $ "=" <> show args
    opLess = \case [v1,v2] -> B $ v1 < v2; args -> error $ "<" <> show args
    opMinus = \case [N n1,N n2] -> N $ n1 - n2; args -> error $ "<" <> show args

newtype Store = Store (Var -> Value)

store0 :: Store
store0 = Store $ \(Var x) -> error $ "store0, " <> x

lookup :: Store -> Var -> Value
lookup (Store s) x = s x

update :: Store -> (Var,Value) -> Store
update s (x,v) = Store $ \k -> if k==x then v else lookup s k
