{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Server.Parser where

import Data.Vector as V((!), toList, fromList, Vector(..))
import Data.HashMap.Strict as HM(HashMap, toList)
import Data.Map as M(Map, fromList)
import Data.Maybe(fromMaybe)
import Data.Aeson
import GHC.Generics(Generic)
import Data.ByteString.Lazy(ByteString)
import Control.Arrow(first)

import Shogi as S(Shogi(..), Result(..))
import Shogi.Const(initialShogi)
import Color(Color(..))
import Board(Board(..), Move(..))
import Board.Const(initialBoard)
import Hands(Hands(..))
import Piece(Kind, fromJKFKindColor)
import Coord(Coord(..))

data Request = Request {
    rule :: Rule,
    shogi :: Shogi,
    move :: Move
} deriving (Show, Generic, FromJSON)

data Response = Response Board (Either S.Result [Move])
              | ErrorResponse String deriving (Generic, ToJSON)

data Rule = Rule deriving (Show) -- TODO

parse :: ByteString -> Maybe Request
parse = decode

stringify :: Maybe Response -> ByteString
stringify = encode. fromMaybe (ErrorResponse "invalid request")

instance FromJSON Shogi where
    parseJSON = withObject "Shogi"$ \o -> do
        turn <- o .: "color"
        board <- o .: "data"
        hands <- o .: "hands"
        return$ initialShogi {
            turn = turn,
            board = board,
            hands = hands
        }
instance ToJSON Shogi where
    toJSON = fail "toJSON Shogi"

instance FromJSON Color where
    parseJSON = withBool "Turn" (\p->return$ if p then Black else White)
instance ToJSON Color

instance FromJSON Hands where
    parseJSON = withArray "Hands"$ \arr -> do
        black <- parseHand$ arr!0
        white <- parseHand$ arr!1
        return$ Hands (black, white)
        where parseHand text = do
                hm <- parseJSON text
                return$ keyToString hm
              keyToString :: HashMap String Int -> Map Kind Int
              keyToString = M.fromList. map (first read). HM.toList
instance ToJSON Hands where
    toJSON = fail "toJSON Hands"

instance FromJSON Board where
    parseJSON obj = do
        vector2d <- withArray "array" (mapM parseRow) obj
        let vector = V.fromList$ concat$ mapM V.toList$ V.toList vector2d

        return$ initialBoard{
            vector = vector
        }
        where parseRow = withArray "row" (mapM parseCell)
              parseCell = withObject "cell"$ \o -> do
                kind <- o .:? "kind"
                color <- o .:? "color"
                return$ do
                    k <- kind
                    c <- color
                    return$ fromJKFKindColor k c
instance ToJSON Board where
    toJSON = fail "toJSON Board"

instance FromJSON Move where
    parseJSON = withObject "move"$ \o -> do
        from <- o .:? "from"
        to <- o .: "to"
        piece <- o .: "piece"
        promote <- o .:? "promote" .!= False
        return$ case from of
            Just f -> Move f to promote
            Nothing -> Put to (read piece)
instance ToJSON Move where
    toJSON = fail "toJSON Move"

instance FromJSON Coord where
    parseJSON = withObject "move"$ \o -> do
        x <- o .: "x"
        y <- o .: "y"
        return$ Coord x y

instance ToJSON S.Result where
    toJSON = fail "toJSON Result"
instance FromJSON Rule where
    parseJSON = withScientific "Rule"$ \num -> return Rule
instance ToJSON Rule where
    toJSON _ = "\"TODO: rule\""
