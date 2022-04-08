module Dict(
  Dict, empty, singleton, isEmpty,
  look, has, put, get, del,
  listDict, mapDict, putList, delList,
  least, greatest
  ) where

empty           :: Dict k v
singleton       :: (k,v) -> Dict k v
isEmpty         :: Dict k v -> Bool
look            :: (Ord k) => Dict k v -> k -> Maybe (k,v)
has             :: (Ord k) => Dict k v -> k -> Bool
put             :: (Ord k) => Dict k v -> (k,v) -> Dict k v
putList         :: (Ord k) => Dict k v -> [(k,v)] -> Dict k v
del             :: (Ord k) => Dict k v -> k -> Dict k v
delList         :: (Ord k) => Dict k v -> [k] -> Dict k v
get             :: (Ord k) => Dict k v -> k -> v
listDict        :: Dict k v -> [(k,v)]
mapDict         :: (Ord k) => (u -> v) -> Dict k u -> Dict k v
least, greatest :: Dict k v -> (k,v)

data Colour = R | B deriving (Read, Show)
data Dict k v = E | T Colour (Dict k v) (k,v) (Dict k v)
  deriving (Read, Show)

empty = E

singleton (k,v) = T B E (k,v) E

isEmpty E = True
isEmpty _ = False

look E _ = Nothing
look (T _ l (x,v) r) k
  | k < x = look l k
  | k > x = look r k
  | True  = Just (x,v)

has t k = maybe False (const True) (look t k)

get t k = case look t k of
  Just (k,v) -> v

put t (k,v) = T B a y b where
  T _ a y b = put' t
  put' E = T R E (k,v) E
  put' s@(T col a (l,w) b) =
    if k < l then bal (T col (put' a) (l,w) b)
    else if k > l then bal (T col a (l,w) (put' b))
    else T col a (l,v) b

bal E = E
bal (T B (T R (T R a x b) y c) z d) = T R (T B a x b) y (T B c z d)
bal (T B (T R a x (T R b y c)) z d) = T R (T B a x b) y (T B c z d)
bal (T B a x (T R (T R b y c) z d)) = T R (T B a x b) y (T B c z d)
bal (T B a x (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
bal (T col a x b) = T col a x b

putList = foldl put 

del t k = fst (del' t) where
  del' E = (E,False)
  del' s@(T c a (l,v) b) = 
    if k < l then bal' c (del' a) (l,v) (b,False)
    else if k > l then bal' c (a,False) (l,v) (del' b)
    else case s of
      T R E _ e -> (e,False)
      T R d _ E -> (d,False)
      T B E _ E -> (E,True)
      T B E _ (T R E x E) -> (T B E x E,False)
      T B (T R E x E) _ E -> (T B E x E,False)
      _ -> let
          (swn,swt) = swapRight (l,v) b
          swapRight x (T c2 E n e) = (n,T c2 E x e)
          swapRight x (T c2 d n e) = (n2,T c2 d2 n e) where
            (n2,d2) = swapRight x d
        in bal' c (a,False) swn (del' swt)
        
  -- balance the branches and fix colour imbalance

  bal' c a x b = (bal y,yh) where (y,yh) = bal'' c a x b

  -- balance the branches, possibly creating a colour imbalance

  bal'' c (a,False) x (b,False) = (T c a x b,False)

  -- right-side-shorter cases

  bal'' c (a,False) x (T R b y d,True) = (T c a x (T B b y d),False)
  bal'' R (T B a x b,False) y (d,True) = (T B (T R a x b) y d,False)
  bal'' B (T B a x b,False) y (d,True) = 
    (bal (T B (T R a x b) y d),True)
  bal'' B (T R (T B n1 n2 n3) n4 (T B n5 n6 n7),False) n8 (n9,True) =
    (T B (bal (T B (T R n1 n2 n3) n4 n5)) n6 (T B n7 n8 n9),False)

  -- left-side-shorter cases

  bal'' c (T R b y d,True) x (a,False) = (T c (T B b y d) x a,False)
  bal'' R (d,True) y (T B a x b,False) = (T B d y (T R a x b),False)
  bal'' B (d,True) y (T B a x b,False) = 
    (bal (T B d y (T R a x b)),True)
  bal'' B (n1,True) n2 (T R (T B n3 n4 n5) n6 (T B n7 n8 n9),False) =
    (T B (T B n1 n2 n3) n4 (bal (T B (T R n5 n6 n7) n8 n9)),False)

delList = foldl del

listDict E = []
listDict (T _ a x b) = (listDict a) ++ (x:(listDict b))

least (T _ E x _) = x
least (T _ a x _) = least a

greatest (T _ _ x E) = x
greatest (T _ _ x b) = greatest b

mapDict f t = putList empty (map (\(k,v)->(k,f v)) (listDict t))

-- testing

verify :: (Ord a) => Dict a b -> (Bool,Int)

verify E = (True,0)
verify s@(T c a x b) = let
    (ac,an) = verify a
    (bc,bn) = verify b
  in case c of
    R -> (ac && bc && not (isRed a) && not (isRed b) && an == bn,an)
    B -> (ac && bc && an == bn,an+1)

isRed (T R _ _ _) = True
isRed _ = False

