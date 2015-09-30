
import qualified Data.Vector as V

type Literal = Int
type Clause = [Literal]
type FormCNF = [Clause]
type Valuation = V.Vector PValue

data PValue = T | F | U
    deriving(Show)

pValueNot :: PValue -> PValue
pValueNot U = U
pValueNot T = F
pValueNot F = T

pValueOr :: PValue -> PValue -> PValue
pValueOr T _ = T
pValueOr _ T = T
pValueOr U _ = U
pValueOr _ U = U
pValueOr F F = F

pValueAnd :: PValue -> PValue -> PValue
pValueAnd F _ = F
pValueAnd _ F = F
pValueAnd U _ = U
pValueAnd _ U = U
pValueAnd T T = T

-- | Evaluate a literal given the valuation
evalLiteral :: Valuation -> Literal -> PValue
evalLiteral v l
    | l > 0     = v V.! (l - 1)
    | l < 0     = pValueNot (v V.! (-l - 1))
    | otherwise = error "Invalid literal"

-- | Evaluate a conjunctive clause 
evalClause :: Valuation -> Clause -> PValue
evalClause v c = foldl pValueOr F (map (evalLiteral v) c)

-- | Evaluate a CNF formula
evalForm :: Valuation -> FormCNF -> PValue
evalForm v f = foldl pValueAnd T (map (evalClause v) f)

c1 :: Clause 
c1 = [1, 2]

c2 :: Clause
c2 = [-1, -2]

f :: FormCNF
f = [c1, c2]

getUnits :: FormCNF -> [Literal]
getUnits f = filter (\c -> 1 == length c) f

-- Literal Tree

data LTree a = Node a (LTree a) (LTree a)
             | Empty deriving (Show)

-- | Generate a literal tree given an array of trees
genLTree :: [Literal] -> LTree Literal
genLTree ls = foldl appendRoot Empty ls

appendRoot :: LTree Literal -> Literal -> LTree Literal
appendRoot t l = Node l t t

-- | Get the left branch, taking the false path
falseBranch :: LTree Literal -> LTree Literal
falseBranch Empty        = Empty
falseBranch (Node a b c) = c

-- | Get the right branch, taking the true path
trueBranch :: LTree Literal -> LTree Literal
trueBranch Empty        = Empty
trueBranch (Node a b c) = c

