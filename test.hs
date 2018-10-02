import Data.Monoid 

data RegExp = Epsilon
            | Concat Char RegExp
            | Union Char RegExp
            | Star RegExp
            deriving (Show)


regex :: String -> RegExp
regex [] = Epsilon
-- regex (x:xs)


data TreeList a = TreeList [TreeList a]
                | Element a
                | Empty
                deriving (Show, Ord, Eq)

instance Monoid (TreeList a) where
  a             `mappend` Empty         = a
  Empty         `mappend` a             = a
  (TreeList a)  `mappend` (TreeList b)  = TreeList (a ++ b)
  (TreeList a)  `mappend` b@(Element _) = TreeList (a ++ [b])
  a@(Element _) `mappend` (TreeList b)  = TreeList ([a] ++ b)
  a@(Element _) `mappend` b@(Element _) = TreeList[a, b]
  mempty                                = Empty

instance Functor TreeList where
  fmap f Empty        = Empty
  fmap f (Element a)  = Element (f a)
  fmap f (TreeList l) = TreeList (fmap (fmap f) l)

safeTail :: String -> String
safeTail "" = ""
safeTail s = tail s

--splitOnFirst :: Char -> String -> String -> (String, Maybe String)
--splitOnFirst c (x:xs) acc
--  | x == c    = (acc, Just xs)
--  | otherwise = splitOnFirst c xs (acc ++ [x])
--splitOnFirst _ [] acc = (acc, Nothing)

splitOnFrist :: Char -> String -> (String, Maybe String)
splitOnFrist c (x:xs)
  | x == c    = ("", Just xs)
  | otherwise = let (bef, aft) = splitOnFrist c xs
                in (x:bef, aft)
splitOnFrist _ [] = ("", Nothing)

--parensAux :: String -> (String, Maybe (String, String))
--parensAux s = case t of
--                Nothing -> (first, Nothing)
--                Just (Nothing, b) -> error $ "parens not ok on \"" ++ s ++ "\""
--                Just (Just a, b)  -> (first, Just (a, b))
--  where (first, after) = splitOnFirst '(' s ""
--        rev = after >>= (Just . reverse)
--        t = rev >>= 
--              (\rev -> Just $ 
--                       (\(a,b) -> (fmap reverse b, reverse a)) .
--                       splitOnFirst ')' rev $ 
--                       "")
--
--getTreeList :: String -> TreeList String
--getTreeList s = 
--  case after of
--    Nothing     -> if first == "" then Empty else Element first
--    Just (a, b) -> TreeList [ Element first
--                            , TreeList [getTreeList a]
--                            , getTreeList b 
--                            ]
--  where (first, after) = parensAux s
