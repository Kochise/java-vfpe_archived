module Datum(
  Datum(..),
  parseList, parseDatum, printDatum
  ) where

import Char

import Dict

-- hide from HUGS
fromInt = fromIntegral

-- data type

data Datum =
    DatumInt    Int
  | DatumDouble Double
  | DatumString String
  | DatumSymbol String
  | DatumList   [Datum]
  | DatumNull 
    deriving (Read, Show)

parseDatum  :: String -> (Datum,String)
parseList    :: String -> (Datum,String)
printDatum  :: Datum -> String

-- parser

parseDatum = (maybe noParse id) . parseDatum'

parseDatum' = parseS
  parseSpace
  (parseAL [
    pTrans parseList' DatumList,
    pTrans parseString DatumString,
    pTrans parseFloat DatumDouble,
    pTrans parseInt DatumInt,
    pTrans parseSymbol DatumSymbol
    ])
  (flip const)

noParse = (DatumNull,[])

parseList = (maybe noParse (\(x,s)->(DatumList x,s))) . parseList'

-- Space
-- NOTE we are using ';' and "{}" comments

parseSpace = parseF () 
  (parseA
    (parseC isSpace (const ()),id)
    (parseComment,id))
  const

parseComment = parseA (parseEOLComment,id) (parseBlockComment,id)

parseEOLComment = parseS
  (parseK ";" ())
  (parseF () (parseC (/= '\n') (const ())) const)
  const

parseBlockComment = parseS
  (parseK "{" ())
  (parseS
    (parseF () parseToken const)
    (parseS parseSpace (parseK "}" ()) const)
    const)
  const

parseToken = parseS
  (parseO parseSpace () id)
  (parseAL [
    pTrans parseString (const ()), 
    pTrans parseFloat (const ()), 
    pTrans parseInt (const ()),
    pTrans parseSymbol (const ()),
    parseK ")" (), 
    parseK "(" ()])
  const

-- Lists

parseList' = parseS
  (parseK "(" ())
  (parseS
    (parseF [] parseDatum' snoc)
    (parseS parseSpace (parseK ")" ()) const)
    const)
  (flip const)

-- Strings

parseString = parseS
  (parseK "\"" ())
  (parseS
    (parseF [] (parseA (parseStrChr,id) (parseEscChar,id)) (++))
    (parseK "\"" ())
    const)
  (flip const)

parseStrChr = parseC (\c->c /= '"' && c /= '\\') single

parseEscChar = parseS
  (parseK "\\" ())
  (parseAL [
    parseC (== '\n') (const []),
    parseC (`elem` specialChar) single,
    parseEscCode ])
  (flip const)

specialChar = "{};()\\\""

parseEscCode = 
  (maybe Nothing (\(i,s)->Just ([chr (i `mod` 256)],s))). parseDec
  
-- Ints

parseInt = parseS (parseO parseSign 1 id) parseDec (*)

parseDec = parseG
  parseDigit
  (parseF 0 parseDigit (\o n->10*o+n))

parseDigit = parseC isDigit digitToInt

parseSign = parseA 
  (parseC (== '+') (const (1::Int)),id)
  (parseC (== '-') (const (-1::Int)),id)
 
-- Doubles

parseFloat = parseS 
  (parseO parseSign 1 id)
  (parseS
    parseDecimal (parseO parseExp 0 id) (\m e->m*10^^e))
  (\s v->(fromIntegral s)*v)

parseDecimal = parseS
  (parseS parseDDec (parseK "." 0) const) 
  (pTrans parseDFrac snd) (+)
    
parseDDec = parseG
  (pTrans parseDigit (const 0.0))
  (parseF 0.0 parseDigit (\o n->10.0*o+(fromIntegral n)))

parseDFrac = parseG
  (pTrans parseDigit (const (0.0,0.0)))
  (parseF (0.1,0.0) parseDigit 
    (\(op,or) n->(op/10.0,or+op*(fromIntegral n))))
  
parseExp = parseS
  (parseA (parseK "e" 0,id) (parseK "E" 0,id))
  parseInt (flip const)
   
-- Symbols (assuming numbers have already been parsed)

parseSymbol = parseS
  parseSymChar (parseF [] parseSymChar (++)) (++)

parseSymChar = parseA (parseEscChar,id) (parseC isSymChar single,id)
  
isSymChar cc = 
  isPrint cc && cc /= ' ' && (not (cc `elem` specialChar))
  
-- printer

printDatum (DatumList l) = '(' : printList l
printDatum (DatumInt i) = show i
printDatum (DatumDouble d) = show d
printDatum (DatumString s) = "\"" ++ concat (map (escape "\\\"") s) ++ "\""
printDatum (DatumSymbol s) = concat (map (escape specialChar) s)

printList [] = ")"
printList [d] = printDatum d ++ ")"
printList (d:ds) = printDatum d ++ " " ++ printList ds

escape ecs c = if c `elem` ecs then '\\' : [c] else [c]
 
snoc xs x = xs ++ [x]
single x = [x]

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

