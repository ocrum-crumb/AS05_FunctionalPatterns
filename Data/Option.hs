{-# LANGUAGE InstanceSigs #-}
module Data.Option where
import Prelude hiding (lookup)

-- An `Option a` models a value of type `a` which may be missing.
data Option a = None | Some a deriving (Show, Eq)

-- Now implement Functor, Applicative and Monad
instance Functor Option where
    fmap :: (a -> b) -> Option a -> Option b
    fmap _ None     = None
    fmap f (Some a) = Some (f a)


instance Applicative Option where
    pure :: a -> Option a
    pure = Some

    (<*>) :: Option (a -> b) -> Option a -> Option b
    None <*> _        = None           -- If the function is None, return None
    _ <*> None        = None           -- If the value is None, return None
    Some f <*> Some a = Some (f a)     -- Apply the function f to the value a

instance Monad Option where
    (>>=) :: Option a -> (a -> Option b) -> Option b
    None >>= _      = None        -- If it's None, return None
    Some a >>= f    = f a         -- If it's Some, apply the function f

-- TODO: Check your definitions by running the tests:
-- > cabal test option --test-show-details=direct

-------------------------------------------------------------------------------
-- Example: Bank accounts
-------------------------------------------------------------------------------

data Account = MkAccount { userId :: Int, balance :: Double }

users :: [(String, Int)]
users = [
  ("daniel.kroeni@fhnw.ch", 42),
  ("hans@noaccou.nt", 43)]

accounts :: [Account]
accounts = [
 MkAccount 1 5,
 MkAccount 41 20,
 MkAccount 42 0]


lookup :: Eq a => a -> [(a,b)] -> Option b
lookup _ [] = None
lookup key ((k,v):kvs) 
  | key == k = Some v
  | otherwise = lookup key kvs


userIdByEmail :: String -> Option Int
userIdByEmail email = lookup email users 


accountByUserId :: Int -> Option Account
accountByUserId uid = lookup uid accountKVs 
  where accountKVs = map (\a -> (userId a, a)) accounts


balanceByEmail' :: String -> Option Double
balanceByEmail' email =
  case userIdByEmail email of
    None -> None
    Some uid -> case accountByUserId uid of
      None -> None
      Some acc -> Some (balance acc)

-- TODO: Implement the same functionality as above using `>>=` 
-- instead of pattern matching:
balanceByEmail'' :: String -> Option Double
balanceByEmail'' email = userIdByEmail email >>= \uid -> accountByUserId uid >>= \acc -> return (balance acc)


-- TODO: Implement the same functionality as above using do-notation:
balanceByEmail :: String -> Option Double
balanceByEmail email = do
  uid <- userIdByEmail email
  acc <- accountByUserId uid
  return (balance acc)

main :: IO ()
main = do
  print $ balanceByEmail "daniel.kroeni@fhnw.ch"

