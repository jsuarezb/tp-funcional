
import qualified Data.Vector as V


type Literal = Int
type Clause = [Literal]
type Formula = [Clause]
type Valuation = V.Vector TruthValue

data TruthValue = F | U | T deriving (Show, Eq)

truthValuetoInt :: TruthValue -> Int
truthValuetoInt F = -1
truthValuetoInt U = 0
truthValuetoInt T = 1

intToTruthValue :: Int -> TruthValue
intToTruthValue (-1) = F
intToTruthValue 0 = U
intToTruthValue 1 = T

orCNF :: TruthValue -> TruthValue -> TruthValue
orCNF a b = intToTruthValue $ max (truthValuetoInt a) (truthValuetoInt b)

andCNF :: TruthValue -> TruthValue -> TruthValue
andCNF a b = intToTruthValue $ min (truthValuetoInt a) (truthValuetoInt b)

notCNF :: TruthValue -> TruthValue
notCNF a = intToTruthValue $ -(truthValuetoInt a)

-- | Evaluate a literal given a valuation
evalLiteral :: Valuation -> Literal -> TruthValue
evalLiteral v l =
    if l > 0
        then v V.! (literalIndex l)
        else notCNF (v V.! (literalIndex l))

-- | Literal index inside the valuation `list`
literalIndex :: Literal -> Int
literalIndex l = (if l > 0 then l else -l) - 1

literalSubindex :: Literal -> Int
literalSubindex l = 1 + literalIndex l

-- | True if the literal has some truth value assigned
-- | inside the valuation, otherwise False
isValuated :: Valuation -> Literal -> Bool
isValuated v l = not $ (evalLiteral v l) == U

-- | Evaluate a CNF clause (disjunctive formula) given a valuation
evalClause :: Valuation -> Clause -> TruthValue
evalClause v c = foldl (\t l -> orCNF (evalLiteral v l) t) F c

isUnitClause :: Valuation -> Clause -> Bool
isUnitClause v c = (== 1) $ length $ filter (not . (== F) . evalLiteral v) c

-- | Max literal index in clause
maxLiteralInClause :: Clause -> Int
maxLiteralInClause c = foldl (\n l -> max n (literalSubindex l)) 0 c

-- | Evaluate a CNF formula (conjunctive formula) given a valuation
evalFormula :: Valuation -> Formula -> TruthValue
evalFormula v f = foldl (\t c -> andCNF (evalClause v c) t) T f

hasUnitClauses :: Valuation -> Formula -> Bool
hasUnitClauses v f = (> 0) $ length $ filter (isUnitClause v) f

maxLiteralInFormula :: Formula -> Int
maxLiteralInFormula f = foldl (\l c -> max (maxLiteralInClause c) l) 0 f

-- | Empty valuation given a formula
emptyValuation :: Formula -> Valuation
emptyValuation f = V.fromList $ map (\_ -> U) [1..maxLiteral]
  where maxLiteral = maxLiteralInFormula f

isSatisfiable :: Formula -> Bool
isSatisfiable f =
    let v = emptyValuation f
    in isSatisfiable' v f 0 F || isSatisfiable' v f 0 T

isSatisfiable' :: Valuation -> Formula -> Int -> TruthValue -> Bool
isSatisfiable' v f i t =
    let nv = v V.// [(i, t)]
        truthValue = evalFormula nv f
        (Just nextVariable) = V.findIndex (\t -> t == U) nv
        leftBranch = isSatisfiable' nv f nextVariable F
        rightBranch = isSatisfiable' nv f nextVariable T
        variablesRemaining = length $ V.filter (\v -> v == U) nv
    in case () of
      _ | truthValue == T -> True
        | truthValue == F -> False
        | otherwise       ->
            if variablesRemaining > 0
                then leftBranch || rightBranch
                else True

c1 :: Clause
c1 = [1, 3]

c2 :: Clause
c2 = [1, -3]

c3 :: Clause
c3 = [-1, 3]

c4 :: Clause
c4 = [-1, -3]

f :: Formula
f = [c1, c2, c3, c4]
