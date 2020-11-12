module AST.DSL where

import           AST

var_ :: n -> Expr (Maybe l) n t
var_ = Var Nothing

ann_ :: Expr (Maybe l) n t -> t -> Expr (Maybe l) n t
ann_ = Ann Nothing

con_ :: n -> Expr (Maybe l) n t
con_ = Con Nothing

hole_ :: n -> Expr (Maybe l) n t
hole_ = Hole Nothing

abs_ :: [n] -> Expr (Maybe l) n t -> Expr (Maybe l) n t
abs_ = Abs Nothing

app_ :: Expr (Maybe l) n t -> Expr (Maybe l) n t -> Expr (Maybe l) n t
app_ = App Nothing

let_
  :: [(n, Expr (Maybe l) n t, Maybe t)]
  -> Expr (Maybe l) n t
  -> Expr (Maybe l) n t
let_ = Let Nothing

case_
  :: Expr (Maybe l) n t -> [(Pat n, Expr (Maybe l) n t)] -> Expr (Maybe l) n t
case_ = Case Nothing

mcase_ :: [([Pat n], Expr (Maybe l) n t)] -> Expr (Maybe l) n t
mcase_ = MCase Nothing

unit_ :: Expr (Maybe l) n t
unit_ = UnitLit Nothing

tuple_ :: [Expr (Maybe l) n t] -> Expr (Maybe l) n t
tuple_ = TupleLit Nothing

list_ :: [Expr (Maybe l) n t] -> Expr (Maybe l) n t
list_ = ListLit Nothing

stringInterp_ :: String -> [(Expr (Maybe l) n t, String)] -> Expr (Maybe l) n t
stringInterp_ = StringInterp Nothing

string_ :: String -> Expr (Maybe l) n t
string_ = StringLit Nothing

char_ :: Char -> Expr (Maybe l) n t
char_ = CharLit Nothing

int_ :: Int -> Expr (Maybe l) n t
int_ = IntLit Nothing

bool_ :: Bool -> Expr (Maybe l) n t
bool_ = BoolLit Nothing

record_ :: [(String, Expr (Maybe l) n t)] -> Expr (Maybe l) n t
record_ = Record Nothing

project_ :: Expr (Maybe l) n t -> String -> Expr (Maybe l) n t
project_ = Project Nothing

fcall_ :: String -> [Expr (Maybe l) n t] -> Expr (Maybe l) n t
fcall_ = FCall Nothing
