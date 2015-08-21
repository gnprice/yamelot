module Indent where

import Data.Char
import qualified Debug.Trace

--trace = Debug.Trace.trace
trace _ = id

type Token = Char
type Range = (Int, Int) -- inclusive
inRange :: Int -> Range -> Bool
inRange i (j, k) = i >= j && i <= k
inf = 9999999
type Parse a = [(Token, Int)]
            -> Range
            -> Maybe (a, [(Token, Int)], Range)

{-
instance Functor Parse where
  fmap ff p cs i = fmap (\(a, cs', i') -> (ff a, cs', i'))
-}

ffmap :: (a -> b) -> Parse a -> Parse b
ffmap ff = fmap $ fmap $ fmap
             (\(a, cs', i') -> (ff a, cs', i'))

data Value = Scalar String
           | Sequence [Value]
  deriving (Show)

epsilon :: Parse ()
epsilon cs i = Just ((), cs, i)

termSatisfy :: (Char -> Bool) -> Parse Char
termSatisfy _ [] _ = Nothing
termSatisfy pred ((c,k):cs) i
    | pred c && k `inRange` i = trace ("term: " ++ show (munge ((c,k):cs), k, i)) $ Just (c, cs, (k, k))
    | otherwise = trace ("term failed: " ++ show (munge ((c,k):cs), k, i)) $ Nothing

term :: Char -> Parse Char
term c = termSatisfy (==c)

munge = map fst

sq :: Parse a -> Parse b -> Parse (a,b)
sq p1 p2 cs i =
    case p1 cs i of
        Nothing -> Nothing
        Just (a, cs', i') -> case p2 cs' i' of
            Nothing -> Nothing
            Just (b, cs'', i'') -> Just ((a,b), cs'', i'')

sql :: Parse a -> Parse b -> Parse a
sql p1 p2 = ffmap fst (p1 `sq` p2)

sqr :: Parse a -> Parse b -> Parse b
sqr p1 p2 = ffmap snd (p1 `sq` p2)

between :: Parse a -> Parse c -> Parse b -> Parse b
between pl pr p = pl `sqr` p `sql` pr

choice :: Parse a -> Parse a -> Parse a
choice p1 p2 cs i =
    case p1 cs i of
        Nothing -> case p2 cs i of
            Nothing -> Nothing
            Just (a, cs', i') -> Just (a, cs', i')
        Just (a, cs', i') -> Just (a, cs', i')

option :: Parse a -> Parse (Maybe a)
option p cs i =
    case p cs i of
        Nothing -> Just (Nothing, cs, i)
        Just (a, cs', i') -> Just (Just a, cs', i')

star :: Parse a -> Parse [a]
star p cs i =
    case p cs i of
        Nothing -> trace ("star empty: " ++ show (munge cs, i)) $ Just ([], cs, i)
        Just (a, cs', i') ->
            case star p cs' i' of
                Nothing -> Nothing
                Just (as, cs'', i'') -> trace ("star: " ++ show (munge cs, munge cs'', i'')) $ Just (a:as, cs'', i'')

data Indent = IGt | IGte | IEq | IAll

indent :: Indent -> Parse a -> Parse a
indent ind p cs i =
    case p cs (fwd ind i) of
        Nothing -> Nothing
        Just (a, cs', i') -> Just (a, cs', i `intersect` bwd ind i')

maxInd :: Parse a -> Parse a
maxInd p cs i =
    case p cs i of
        Nothing -> Nothing
        Just (a, cs', (_,ir)) | ir == inf -> Nothing
                              | otherwise -> Just (a, cs', (ir,ir))

intersect (l,h) (l',h') = (max l l', min h h')

plus p cs i = case (p `sq` star p) cs i of
    Nothing -> Nothing
    Just ((t,ts), cs', i') -> Just (t:ts, cs', i')

gt = indent IGt
eq = indent IEq
iall = indent IAll
gte = indent IGte

fwd IGt (l,h) = (l+1,inf)
fwd IGte (l,h) = (l,inf)
fwd IEq (l,h) = (l,h)
fwd IAll (l,h) = (0,inf)

bwd IGt (l,h) = (0,h-1)
bwd IGte (l,h) = (0,h)
bwd IEq (l,h) = (l,h)
bwd IAll (l,h) = (0,inf)

startIx = 0
ann :: String -> [(Char, Int)]
ann xs = go startIx xs
    where go _ [] = []
          go i ('\n':xs) = ('\n', i) : go startIx xs
          go i (' ':xs) = (' ', i) : go (i+1) xs
          go i (x:xs) = (x, i) : go (i+1) xs

run :: String -> Parse a -> Maybe (a, String)
run cs p = fmap (\(t,xs,_) -> (t,munge xs)) (p (ann cs) (0, inf))

eat = sws . gte
tok = eat . term

ws = star $ iall $ termSatisfy (flip elem " \n")
sws = (`sql` ws)
word = maxInd $ plus $ gte $ termSatisfy isAlphaNum

list = ffmap Sequence $ plus (eq item)
item = sws (eq (term '-')) `sqr` (other `choice` gt list)
other = ffmap Scalar (gt $ eat word) `choice` flow_collection
flow_collection = flow_list
flow_list = ffmap Sequence $ between (gt $ tok '[') (tok ']') $
    ffmap coalesce $ option $
        (eat flow)
            `sq` star ((tok ',') `sqr` (eat flow))
            `sql` option (tok ',')
  where coalesce Nothing = []
        coalesce (Just (v,vs)) = v:vs
flow = ffmap Scalar word `choice` flow_collection

yamelot = list
