{-# LANGUAGE DeriveGeneric #-}
-- | Color of players
module Color(Color(..), opposite) where

import GHC.Generics(Generic)

-- | Color of players
data Color = Black | White deriving (Eq, Generic)

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
