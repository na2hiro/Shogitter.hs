module Color(Color(..), opposite) where

data Color = Black | White deriving (Eq)
instance Show Color where
    show Black = "+"
    show White = "-"

opposite :: Color -> Color
opposite Black = White
opposite White = Black
