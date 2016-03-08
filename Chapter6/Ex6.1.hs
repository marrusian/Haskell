import Prelude hiding (snd)

-- 6.1)
snd :: (a,b) -> b
snd (x,y) = y

sing :: a -> [a]
sing x = [x]

-- 6.2)
-- [[a]] -> [[a]] is an instance of the type [a'] -> [a'] (a' is being replaced by [a]),
-- so the most general type (MGT) is [a'] -> [a']

-- 6.3)
shift :: ((a,a),a) -> (a,(a,a))
shift ((x,y),z) = (x,(y,z))
