module Indent where

import qualified Debug.Trace

--trace = Debug.Trace.trace
trace _ = id

type Token = Char
type Range = (Int, Int) -- inclusive
inRange :: Int -> Range -> Bool
inRange i (j, k) = i >= j && i <= k
inf = 9999999
type Parse = [(Token, Int)]
          -> Range
          -> Bool -- absolute alignment
          -> Maybe (Tree, [(Token, Int)], Range, Bool)

data Tree = Epsilon
          | Terminal Char
          | Lookahead
          | Sequence Tree Tree
          | ChoiceLeft Tree
          | ChoiceRight Tree
          | Star [Tree]
    deriving (Show)

epsilon :: Parse
epsilon cs i f = Just (Epsilon, cs, i, f)

term :: Char -> Parse
term _ [] _ _ = Nothing
term a ((c,k):cs) i f
    | a == c && k `inRange` i = trace ("term: " ++ show (munge ((c,k):cs), k, i)) $ Just (Terminal c, cs, (k, k), False)
    | otherwise = trace ("term failed: " ++ show (munge ((c,k):cs), k, i)) $ Nothing

munge = map fst

sq :: Parse -> Parse -> Parse
sq p1 p2 cs i f =
    case p1 cs i f of
        Nothing -> Nothing
        Just (t, cs', i', f') -> case p2 cs' i' f' of
            Nothing -> Nothing
            Just (t', cs'', i'', f'') -> Just (Sequence t t', cs'', i'', f'')

lookahead :: Parse -> Parse
lookahead p cs i f =
    case p cs i f of
        Nothing -> Nothing
        Just (_, _, _, _) -> Just (Lookahead, cs, i, f)

choice :: Parse -> Parse -> Parse
choice p1 p2 cs i f =
    case p1 cs i f of
        Nothing -> case p2 cs i f of
            Nothing -> Nothing
            Just (t, cs', i', f') -> Just (ChoiceRight t, cs', i', f')
        Just (t, cs', i', f') -> Just (ChoiceLeft t, cs', i', f')

star :: Parse -> Parse
star p cs i f =
    case p cs i f of
        Nothing -> trace ("star empty: " ++ show (munge cs, i)) $ Just (Star [], cs, i, f)
        Just (t, cs', i', f') ->
            case star p cs' i' f' of
                Nothing -> Nothing
                Just (Star ts, cs'', i'', f'') -> trace ("star: " ++ show (munge cs, munge cs'', i'')) $ Just (Star (t:ts), cs'', i'', f'')

indent :: Indent -> Parse -> Parse
indent ind p cs i False =
    trace ("indent: " ++ show (munge cs, i)) $
    case p cs (fwd ind i) False of
        Nothing -> Nothing
        Just (t, cs', i', f') -> trace ("indent exit: " ++ show (munge cs', i', i `intersect` bwd ind i')) $ Just (t, cs', i `intersect` bwd ind i', f')
indent ind p cs i True = trace ("indent with pipe: " ++ show (munge cs, i)) $ p cs i True

fixFirst :: Parse -> Parse
fixFirst p cs i f = p cs i True

data Indent = IGt | IGte | IEq | IAll

intersect (l,h) (l',h') = (max l l', min h h')

plus p cs i f = case (p `sq` star p) cs i f of
    Nothing -> Nothing
    Just (Sequence t (Star ts), cs', i', f') -> Just (Star (t:ts), cs', i', f')

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
          go i ('\n':xs) = go startIx xs
          go i (' ':xs) = go (i+1) xs
          go i (x:xs) = (x, i) : go (i+1) xs

run :: String -> Parse -> Maybe (Tree, String)
run cs p = fmap (\(t,xs,_,_) -> (t,munge xs)) (p (tok cs) (0, inf) False)


gram3 cs i f = trace ("gram3: " ++ show i) $ case
        (term 'a' `sq` gt (star (fixFirst (indent IEq gram3))))
        cs i f of
    Nothing -> trace "gram3 failed" $ Nothing
    r@(Just (t, cs', i', f')) -> trace ("gram3 exit: " ++ show r) $ r

gterm = gt . term

item = eq (term '-') `sq` (other `choice` gt list)
other = gterm 'a' `choice` flow_collection
list = plus (eq item)
flow_collection = flow_list
flow_list = gterm '[' `sq` star (gte flow) `sq` gte (gterm ']')
flow = term 'b' `choice` flow_collection

yamelot = list
