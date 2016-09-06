main :: IO()
main = {- do -}
  print "hi"
--   print $ 1 + 2
--   print 10
--   print $ negate (-1)
--   print ((+) 0 blah)
--     where blah = negate 1

-- polymorph1 :: a -> a -> a
-- polymorph1 x y = id x
-- polymorph x y = y

-- polymorph2 :: a -> b -> b
-- polymorph2 _ y = y

-- data Mood = Blah | Woot deriving Show
-- changeMood :: Mood -> Mood
-- changeMood Blah = Woot
-- changeMood    _ = Blah
--
-- type Name = String
--
-- data Pet = Cat | Dog Name

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==)   _   _ = False
