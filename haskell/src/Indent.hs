module Indent where

import Data.Char
import qualified Debug.Trace
import Control.Applicative
import Control.Monad

--trace = Debug.Trace.trace
trace _ = id

-- | Since this is a scanner-less parser, our tokens are just
-- characters.
type Token = Char

-- | An inclusive range between two integers.
type Range = (Int, Int) -- inclusive

-- | Tests if @i@ in 'Range'.
inRange :: Int -> Range -> Bool
inRange i (j, k) = i >= j && i <= k

-- | Hack to represent an infinite integer (so this will break if
-- we have any columns greater than this volume).  Proper way to
-- do this is have a proper data type for integers plus infinity.
inf :: Int
inf = 99999999

newtype Parse a =
    Parse {
        runParse :: [(Token, Int)]
                 -> Range
                 -> Maybe (a, [(Token, Int)], Range) }

instance Monad Parse where
    return x = Parse $ \cs i -> Just (x, cs, i)
    m >>= f = Parse $ \cs i ->
        -- TODO: better to factor this out so we can do both loose and
        -- lock
        case runParse m cs i of
            Nothing -> Nothing
            Just (a, cs', i') -> case runParse (f a) cs' (followLock Loose i i') of
                Nothing -> Nothing
                Just (b, cs'', i'') -> Just (b, cs'', afterLock Loose i' i'')

instance Applicative Parse where
    pure = return
    (<*>) = ap

instance Alternative Parse where
    empty = Parse $ \_ _ -> Nothing
    p1 <|> p2 = choice p1 p2

instance Functor Parse where
  fmap ff p = Parse $ \cs i -> fmap (\(a, cs', i') -> (ff a, cs', i')) (runParse p cs i)
data Value = Scalar String
           | Sequence [Value]
  deriving (Show)

epsilon :: Parse ()
epsilon = Parse $ \cs i -> Just ((), cs, i)

termSatisfy :: (Char -> Bool) -> Parse Char
termSatisfy pred = Parse go
    where go [] _ = Nothing
          go ((c,k):cs) i
            | pred c && k `inRange` i
            = trace ("term: " ++ show (munge ((c,k):cs), k, i)) $ Just (c, cs, (k, k))
            | otherwise
            = trace ("term failed: " ++ show (munge ((c,k):cs), k, i)) $ Nothing

term :: Char -> Parse Char
term c = termSatisfy (==c)

munge = map fst

data Lock = Lock | Loose

followLock :: Lock -> Range -> Range -> Range
followLock Lock  _ inner = inner
followLock Loose _ _     = (0, inf)

afterLock :: Lock -> Range -> Range -> Range
afterLock Lock  _     inner = inner
afterLock Loose outer _     = outer

sqAny :: Lock -> Parse a -> Parse b -> Parse (a,b)
sqAny l p1 p2 = Parse $ \cs i ->
    case runParse p1 cs i of
        Nothing -> Nothing
        Just (a, cs', i') -> case runParse p2 cs' (followLock l i i') of
            Nothing -> Nothing
            Just (b, cs'', i'') -> Just ((a,b), cs'', afterLock l i' i'')

sq = sqAny Loose
sqLock = sqAny Lock

sql :: Parse a -> Parse b -> Parse a
sql p1 p2 = fmap fst (p1 `sq` p2)

sqr :: Parse a -> Parse b -> Parse b
sqr p1 p2 = fmap snd (p1 `sq` p2)

between :: Parse a -> Parse c -> Parse b -> Parse b
between pl pr p = pl `sqr` p `sql` pr

choice :: Parse a -> Parse a -> Parse a
choice p1 p2 = Parse $ \cs i ->
    case runParse p1 cs i of
        Nothing -> case runParse p2 cs i of
            Nothing -> Nothing
            Just (a, cs', i') -> Just (a, cs', i')
        Just (a, cs', i') -> Just (a, cs', i')

option :: Parse a -> Parse (Maybe a)
option p = Parse $ \cs i ->
    case runParse p cs i of
        Nothing -> Just (Nothing, cs, i)
        Just (a, cs', i') -> Just (Just a, cs', i')

starAny :: Lock -> Parse a -> Parse [a]
starAny l p = Parse $ \cs i ->
    case runParse p cs i of
        Nothing -> trace ("star empty: " ++ show (munge cs, i)) $ Just ([], cs, i)
        Just (a, cs', i') ->
            case runParse (starAny l p) cs' (followLock l i i') of
                Nothing -> Nothing
                Just (as, cs'', i'') -> trace ("star: " ++ show (munge cs, munge cs'', i'')) $ Just (a:as, cs'', afterLock l i' i'')

star = starAny Loose
starLock = starAny Lock


data Indent = IGt | IGte

indent :: Indent -> Parse a -> Parse a
indent ind p = Parse $ \cs i ->
    case runParse p cs (fwd ind i) of
        Nothing -> Nothing
        Just (a, cs', i') -> Just (a, cs', i `intersect` bwd ind i')

maxInd :: Parse a -> Parse a
maxInd p = Parse $ \cs i ->
    case runParse p cs i of
        Nothing -> Nothing
        Just (a, cs', (_,ir)) | ir == inf -> Nothing
                              | otherwise -> Just (a, cs', (ir,ir))

intersect (l,h) (l',h') = (max l l', min h h')

plus p = Parse $ \cs i -> case runParse (p `sq` star p) cs i of
    Nothing -> Nothing
    Just ((t,ts), cs', i') -> Just (t:ts, cs', i')

plusLock p = Parse $ \cs i -> case runParse (p `sqLock` starLock p) cs i of
    Nothing -> Nothing
    Just ((t,ts), cs', i') -> Just (t:ts, cs', i')

sepEndBy :: Parse a -> Parse b -> Parse [b]
sepEndBy sep elt =
    fmap coalesce $ option $
        elt `sq` star (sep `sqr` elt) `sql` option sep
  where coalesce Nothing = []
        coalesce (Just (v,vs)) = v:vs

gt = indent IGt
gte = indent IGte

fwd IGt (l,h) = (l+1,inf)
fwd IGte (l,h) = (l,inf)

bwd IGt (l,h) = (0,h-1)
bwd IGte (l,h) = (0,h)

startIx = 0
ann :: String -> [(Char, Int)]
ann xs = go startIx xs
    where go _ [] = []
          go i ('\n':xs) = ('\n', i) : go startIx xs
          go i (' ':xs) = (' ', i) : go (i+1) xs
          go i (x:xs) = (x, i) : go (i+1) xs

run :: String -> Parse a -> Maybe (a, String)
run cs p = fmap (\(t,xs,_) -> (t,munge xs)) (runParse p (ann cs) (0, inf))

{-
Possible next hard parts:
* literal scalars
* literal scalars, with explicit indentation
* more-complete plain scalars

Easy(??) parts that will cover a bunch more tests:
* mappings
* numbers
-}

ws = star $ termSatisfy (flip elem " \n")
eat = (`sql` ws)
tok = eat . term

flow_scalar = fmap Scalar $ eat $ maxInd $ plus $ termSatisfy isAlphaNum
flow_list = fmap Sequence $ between (tok '[') (tok ']') $
    sepEndBy (tok ',') flow_node
flow_collection = flow_list
flow_node = flow_scalar `choice` flow_collection

block_scalar = flow_scalar
block_list = fmap Sequence $ plusLock item
  where item = fmap snd $ tok '-' `sqLock`
                 gt ( block_list
             `choice` block_scalar
             `choice` flow_collection
                    )

yamelot = block_list
