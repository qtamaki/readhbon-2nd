data ZipList a = ZipList [a]

data ZipList a = ZipList { getZipList :: [a] }

newtype ZipList a = ZipList { getZipList :: [a] }

data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession

newtype CharList = CharList { getCharList :: [Char] } deriving {Eq, Show}

CharList :: [Char] -> CharList

getCharList :: CharList -> [Char]

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap f (Pair (x,y)) = Pair (f x, y)

data CoolBool = CoolBool {getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

newtype CoolBool = CoolBool { getCoolBool :: Bool }


