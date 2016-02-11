-- | Color of players
module Color(Color(..), opposite) where

-- | Color of players
data Color = Black | White deriving (Eq)

-- |
-- >>> Black
-- +
-- >>> White
-- -
instance Show Color where
    show Black = "+"
    show White = "-"

-- |
-- get opposite color
--
-- >>> opposite Black == White
-- True
-- >>> opposite White == Black
-- True
opposite :: Color -> Color
opposite Black = White
opposite White = Black
