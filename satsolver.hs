
import qualified Data.Vector as V
import qualified System.Environment as SE
import qualified Data.List.Split as Split
import qualified Data.List as List

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
notCNF a = case a of
    F -> T
    U -> U
    T -> F

-- | Literal index inside the valuation `list`
literalIndex :: Literal -> Int
literalIndex l
    | l > 0     = l - 1
    | l < 0     = -(l + 1)
    | otherwise = error "Invalid literal: can't have 0 as a literal index"

-- | Max literal index in clause
maxLiteralInClause :: Clause -> Int
maxLiteralInClause c = foldl (\p l -> max ((literalIndex l) + 1) p) 0 c

maxLiteralInFormula :: Formula -> Int
maxLiteralInFormula f = foldl (\l c -> max (maxLiteralInClause c) l) 0 f

-- | Evaluate a literal given a valuation
-- | If literal is not defined in the valuation, it returns undefined
evalLiteral :: Valuation -> Literal -> TruthValue
evalLiteral v l =
    let value = do
        i <- Just (literalIndex l)
        t <- v V.!? i
        return (if l > 0 then t else notCNF t)
    in case value of
        Nothing -> U
        Just x  -> x

-- | Evaluate a CNF clause (disjunctive formula) given a valuation
evalClause :: Valuation -> Clause -> TruthValue
evalClause v c = foldl (\p literal -> orCNF p (evalLiteral v literal)) F c

-- | Evaluate a CNF formula (conjunctive formula) given a valuation
evalFormula :: Valuation -> Formula -> TruthValue
evalFormula v form =
    let f p clause = andCNF p (evalClause v clause)
    in foldl f T form

-- | True if the literal has some truth value assigned
-- | inside the valuation, otherwise False
isValuated :: Valuation -> Literal -> Bool
isValuated v l = evalLiteral v l /= U

isUnitClause :: Valuation -> Clause -> Bool
isUnitClause v c =
  let f t = (/= F) $ (evalLiteral v t)
  in (== 1) $ length $ filter f c

unitClauseLiteral :: Valuation -> Clause -> Maybe Int
unitClauseLiteral v c =
    let truthValue t = evalLiteral v t
        literals = filter ((/= F) . truthValue) c
        size = length literals
    in if (length literals) == 1
        then Just (head literals)
        else Nothing

undecidedClauses :: Valuation -> Formula -> Formula
undecidedClauses v f = filter (\x -> (evalClause v x) /= T) f

formUnitClauseLiterals :: Valuation -> Formula -> [Maybe Int]
formUnitClauseLiterals v f = filter (\x -> x /= Nothing) $ map (unitClauseLiteral v) f

-- | Empty valuation given a formula
-- | ensures to map every variable with F, U or T
emptyValuation :: Formula -> Valuation
emptyValuation f = V.fromList $ map (\_ -> U) [1..maxLiteral]
  where maxLiteral = maxLiteralInFormula f

-- | Function to get satisfiability of a formula
isSatisfiable :: Formula -> Bool
isSatisfiable f =
    let valuation = emptyValuation f
    in isSatisfiable' valuation f 0 F || isSatisfiable' valuation f 0 T

-- | Auxiliar function that decides whether a formula is satisfiable
-- | with a given partial valuation and a new truth value to an indexed variable
isSatisfiable' :: Valuation -> Formula -> Int -> TruthValue -> Bool
isSatisfiable' v f i t =
    let valuation = v V.// [(i, t)]
        reducedFormula = undecidedClauses valuation f
        unitLiterals = formUnitClauseLiterals valuation reducedFormula
        nextVariable = case unitLiterals of
            []            -> V.findIndex (\x -> x == U) valuation
            ((Just l):ls) -> Just (literalIndex l)
        truthValue = evalFormula valuation reducedFormula
    in case truthValue of
        T -> True
        F -> False
        _ -> case nextVariable of
            Nothing -> error "No vars"
            Just x  -> isSatisfiable' valuation reducedFormula x F || isSatisfiable' valuation reducedFormula x T


-- TESTING PURPOUSE --

c1 :: Clause
c1 = [1, 2, 3]

c2 :: Clause
c2 = [1, -2, 3]

c3 :: Clause
c3 = [-1, 2]

c4 :: Clause
c4 = [-1, -2]

f :: Formula
f = [c1]

main = do
    args <- SE.getArgs
    content <- readFile (args !! 0)
    ls <- return (Prelude.lines content)
    putStr $ show $ isSatisfiable $ getFormula (tail ls)
    return ()
  where getFormula ls = map lineToClause ls

lineToClause line = filter (\x -> x /= 0) $ map (\x -> read x :: Int) $ (Split.splitOn " " line)
