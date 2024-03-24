module Kripke where

import Data.List (foldl', intercalate)
import Data.Map (Map)
import Data.Map qualified as M
import Text.Parsec

type Term = String

type World = String

data Prop where
  Var :: Term -> Prop
  And :: Prop -> Prop -> Prop
  Or :: Prop -> Prop -> Prop
  Not :: Prop -> Prop
  Impl :: Prop -> Prop -> Prop

instance Show Prop where
  show (Var s) = s
  show (Or p q) = "(" ++ show p ++ ") ∨ (" ++ show q ++ ")"
  show (And p q) = "(" ++ show p ++ ") ∧ (" ++ show q ++ ")"
  show (Not p) = "¬(" ++ show p ++ ")"
  show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"

isVar :: Prop -> Bool
isVar (Var _) = True
isVar _ = False

type WorldOrder = String -> String -> Bool

showWorld :: Map String [String] -> String
showWorld wrld =
  let worldLabels = M.keys wrld
      varLabels x = x ++ " ⊨ᵥ " ++ intercalate ", " (wrld M.! x)
   in unlines (map varLabels worldLabels)

compareWorld :: WorldOrder -> String -> String -> String
compareWorld wo w v
  | wo w v = w ++ " < " ++ v
  | wo v w = v ++ " < " ++ w
  | otherwise = ""

showOrd :: [String] -> WorldOrder -> String
showOrd (x : xs) wo = showOrd xs wo ++ intercalate ", " (map (compareWorld wo x) xs)
showOrd [] _ = ""

data KripkeModel = Kripke {worlds :: !(Map World [Term]), ordering :: !WorldOrder}

instance Show KripkeModel where
  show (Kripke wrld ord) = showWorld wrld ++ showOrd (M.keys wrld) ord

defaultOrdering :: String -> String -> Bool
defaultOrdering x y = x == y

extend :: WorldOrder -> (String, String) -> WorldOrder
extend wo (x, y) a b = (x == a && y == b) || wo a b

emptyModel :: KripkeModel
emptyModel = Kripke M.empty defaultOrdering

addWorld :: KripkeModel -> (World, [Term]) -> KripkeModel
addWorld (Kripke univ ord) (wrld, vars) = Kripke (M.insert wrld vars univ) ord

addOrd :: KripkeModel -> (World, World) -> KripkeModel
addOrd (Kripke univ ord) newarrow = Kripke univ (extend ord newarrow)

getFutures :: KripkeModel -> World -> [World]
getFutures (Kripke univ ord) wrld = filter (ord wrld) (M.keys univ)

validVar :: Term -> KripkeModel -> World -> Bool
validVar s (Kripke univ _) wrld = maybe False (s `elem`) (M.lookup wrld univ)

valid :: Prop -> KripkeModel -> World -> Bool
valid (Var s) km = validVar s km
valid (And p q) km = (&&) <$> valid p km <*> valid q km
valid (Or p q) km = (||) <$> valid p km <*> valid q km
valid (Not p) km = not . any (valid p km) . getFutures km
valid (Impl p q) km = all (valid q km) . filter (valid p km) . getFutures km

readWorldTerm :: String -> (World, [Term])
readWorldTerm s =
  let wrds = words s
   in case wrds of
        [] -> error "Line blank"
        (x : xs) -> (x, drop 1 xs)

readWorldOrd :: String -> (World, World)
readWorldOrd s =
  let wrds = words s
   in case wrds of
        [] -> error "Line blank"
        [x, _, z] -> (x, z)
        _anyOtherFailure -> error "Incorrectly formatted readWorldOrd"

readLine :: String -> Either (World, [Term]) (World, World)
readLine s =
  if '<' `elem` s
    then Right (readWorldOrd s)
    else Left (readWorldTerm s)

addToModel :: KripkeModel -> Either (World, [Term]) (World, World) -> KripkeModel
addToModel km (Left wt) = addWorld km wt
addToModel km (Right ot) = addOrd km ot

-- This is particularly lazy, but it is also fast to write.
processFile :: String -> KripkeModel
processFile = foldl' addToModel emptyModel . map readLine . lines

loadKripkeModel :: String -> IO KripkeModel
loadKripkeModel s = processFile <$> readFile s

