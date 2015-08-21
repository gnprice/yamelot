module Indent where

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
            -> Bool -- absolute alignment
            -> Maybe (a, [(Token, Int)], Range, Bool)

{-
instance Functor Parse where
  fmap ff p cs i f = fmap (\(a, cs', i', f') -> (ff a, cs', i', f'))
-}

ffmap :: (a -> b) -> Parse a -> Parse b
ffmap ff = fmap $ fmap $ fmap $ fmap
             (\(a, cs', i', f') -> (ff a, cs', i', f'))

data Value = Scalar Char
           | Sequence [Value]
  deriving (Show)

epsilon :: Parse ()
epsilon cs i f = Just ((), cs, i, f)

term :: Char -> Parse Char
term _ [] _ _ = Nothing
term a ((c,k):cs) i f
    | a == c && k `inRange` i = trace ("term " ++ show a ++ ": " ++ show (munge ((c,k):cs), k, i)) $ Just (c, cs, (k, k), False)
    | otherwise = trace ("term " ++ show a ++ " failed: " ++ show (munge ((c,k):cs), k, i)) $ Nothing

munge = map fst

sq :: Parse a -> Parse b -> Parse (a,b)
sq p1 p2 cs i f =
    case p1 cs i f of
        Nothing -> Nothing
        Just (a, cs', i', f') -> case p2 cs' i' f' of
            Nothing -> Nothing
            Just (b, cs'', i'', f'') -> Just ((a,b), cs'', i'', f'')

sql :: Parse a -> Parse b -> Parse a
sql p1 p2 = ffmap fst (p1 `sq` p2)

sqr :: Parse a -> Parse b -> Parse b
sqr p1 p2 = ffmap snd (p1 `sq` p2)

choice :: Parse a -> Parse a -> Parse a
choice p1 p2 cs i f =
    case p1 cs i f of
        Nothing -> case p2 cs i f of
            Nothing -> Nothing
            Just (a, cs', i', f') -> Just (a, cs', i', f')
        Just (a, cs', i', f') -> Just (a, cs', i', f')

star :: Parse a -> Parse [a]
star p cs i f =
    case p cs i f of
        Nothing -> trace ("star empty: " ++ show (munge cs, i)) $ Just ([], cs, i, f)
        Just (a, cs', i', f') ->
            case star p cs' i' f' of
                Nothing -> Nothing
                Just (as, cs'', i'', f'') -> trace ("star: " ++ show (munge cs, munge cs'', i'')) $ Just (a:as, cs'', i'', f'')

indent :: Indent -> Parse a -> Parse a
indent ind p cs i False =
    case p cs (fwd ind i) False of
        Nothing -> Nothing
        Just (a, cs', i', f') -> Just (a, cs', i `intersect` bwd ind i', f')
indent ind p cs i True = p cs i True

fixFirst :: Parse a -> Parse a
fixFirst p cs i f = p cs i True

data Indent = IGt | IGte | IEq | IAll

intersect (l,h) (l',h') = (max l l', min h h')

plus p cs i f = case (p `sq` star p) cs i f of
    Nothing -> Nothing
    Just ((t,ts), cs', i', f') -> Just (t:ts, cs', i', f')

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
tok :: String -> [(Char, Int)]
tok xs = go startIx xs
    where go _ [] = []
          go i ('\n':xs) = ('\n', i) : go startIx xs
          go i (' ':xs) = (' ', i) : go (i+1) xs
          go i (x:xs) = (x, i) : go (i+1) xs

run :: String -> Parse a -> Maybe (a, String)
run cs p = fmap (\(t,xs,_,_) -> (t,munge xs)) (p (tok cs) (0, inf) False)

gterm = gt . term

ws = star $ iall $ term ' ' `choice` term '\n'

list = ffmap Sequence $ plus (eq item)
item = eq (term '-') `sqr` ws `sqr` (other `choice` gt list) `sql` ws
other = ffmap Scalar (gterm 'a') `choice` flow_collection
flow_collection = flow_list
flow_list = ffmap Sequence $ gterm '[' `sqr` ws `sqr` star (gte flow `sql` ws) `sql` gte (gterm ']')
flow = ffmap Scalar (term 'b') `choice` flow_collection

yamelot = list
