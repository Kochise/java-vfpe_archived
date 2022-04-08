
module ParserCombinators(
  Parser,
  parseO, parseS, parseA, parseC, parseK, parseG,
  parseF, parseAL) where

type Parser a = String -> Maybe (a,String)
parseO  :: Parser a -> b -> (a -> b) -> Parser b
parseS  :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
parseA  :: (Parser a, a -> c) -> (Parser b , b -> c) -> Parser c
parseC  :: (Char -> Bool) -> (Char -> a) -> Parser a
parseK  :: String -> a -> Parser a
parseF  :: b -> Parser a -> (b -> a -> b) -> Parser b
parseG  :: Parser a -> Parser a -> Parser a
parseAL :: [Parser a] -> Parser a

parseO pa z f s = maybe (Just (z,s)) phase1 (pa s) where
  phase1 (a,t) = Just (f a,t)

parseS pa pb f s = phase1 where
  phase1 = maybe Nothing phase2 (pa s) where
    phase2 (a,t) = maybe Nothing phase3 (pb t) where
      phase3 (b,u) = Just (f a b,u)
        
parseA (pa,af) (pb,bf) s = phase1 where
  phase1 = maybe phase2 phase3 (pa s) where
    phase3 (a,t) = Just (af a,t)
    phase2 = maybe Nothing phase4 (pb s) where
      phase4 (b,u) = Just (bf b,u)
      
parseC p f [] = Nothing
parseC p f (c:cs) = if p c then Just (f c,cs) else Nothing

parseK k z s = 
  if isPrefix k s 
  then Just (z,drop (length k) s) 
  else Nothing
  
parseF z p f s = maybe (Just (z,s)) phase2 (p s) where
  phase2 (a,t) = parseF (f z a) p f t
  
parseG g f s = maybe Nothing (const (f s)) (g s)

parseAL = foldl1 (\p1 p2->parseA (p1,id) (p2,id))
  
pTrans p f s = maybe Nothing (\(x,t)->Just (f x,t)) (p s)

isPrefix [] s = True
isPrefix (c:cs) [] = False
isPrefix (k:ks) (s:ss) = k == s && isPrefix ks ss

