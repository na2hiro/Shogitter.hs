{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Server.Parser where

import Data.Vector as V((!), toList, fromList, Vector(..), length, head, cons, take, drop, singleton)
import Data.HashMap.Strict as HM(HashMap, toList)
import Data.Map as M(Map, fromList, mapKeys)
import Data.Maybe(fromMaybe)
import Data.Aeson
import GHC.Generics(Generic)
import Data.ByteString.Lazy(ByteString)
import Control.Arrow(first)
import Data.Foldable(asum)
import Control.Monad(liftM)

import Shogi as S(Shogi(..), Result(..))
import Shogi.Const(initialShogi)
import Color(Color(..))
import Board(Board(..), Move(..))
import Board.Const(initialBoard)
import Hands(Hands(..))
import Piece(Kind, fromJKFKindColor, toJKFKindColor, Piece(..))
import Coord(Coord(..))
import Rule(RuleConfig(..), defaultRuleConfig, fromMap)

data Request = Request {
    rule :: RuleConfig,
    shogi :: Shogi,
    move :: Move
} deriving (Show, Generic, FromJSON)

data Response = Response {
    newShogi :: Shogi,
    next :: Either S.Result [Move]
} | ErrorResponse {
    error :: String
} deriving (Generic, ToJSON, Show)

parse :: ByteString -> Maybe Request
parse = decode

stringify :: Maybe Response -> ByteString
stringify = encode. fromMaybe (ErrorResponse "cannot stringify")

-- TODO include rule?
instance FromJSON Shogi where
    parseJSON = withObject "Shogi"$ \o -> do
        turn <- o .: "color"
        board <- o .: "board"
        hands <- o .: "hands"
        return$ initialShogi {
            turn = turn,
            board = board,
            hands = hands
        }
-- TODO include rule?
instance ToJSON Shogi where
    toJSON Shogi{hands=hands, board=board, turn=turn} = object [
        "board" .= toJSON board,
        "hands" .= toJSON hands,
        "color" .= toJSON turn]

instance FromJSON Color where
    parseJSON = withScientific "Turn"$ \n->case n of
        0 -> return Black
        1 -> return White
        _ -> fail "Color should be 0 or 1"
instance ToJSON Color where
    toJSON Black = Number 0
    toJSON White = Number 1

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
-- TODO: Kind should be parsed/stringified correctly. toUpperCase etc
instance ToJSON Hands where
    toJSON (Hands (hand0, hand1)) = toJSON [
        toJSON hand0',
        toJSON hand1']
        where hand0' = mapKeys show hand0
              hand1' = mapKeys show hand1

instance FromJSON Board where
    parseJSON obj = do
        vector2d <- withArray "array" (mapM parseRow) obj
        let xSize = V.length vector2d
        let ySize = V.length$ V.head vector2d
        let vector = V.fromList$ concatMap V.toList$ V.toList vector2d

        return$ initialBoard{
            size = (xSize, ySize),
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
    toJSON Board{size = size, vector = vector} = toArray$ unconcat size$ fmap cellToJSON vector
        where cellToJSON :: Maybe Piece -> Value
              cellToJSON Nothing = object []
              cellToJSON (Just p) = let (kind, color) = toJKFKindColor p
                                    in object ["kind" .= kind, "color" .= toJSON color]
              unconcat :: (Int, Int) -> Vector Value -> Vector (Vector Value)
              unconcat s@(_, y) v = if null next
                                        then V.singleton$ V.take y v
                                        else V.take y v `cons` unconcat s next
                  where next = V.drop y v
              toArray :: Vector (Vector Value) -> Value
              toArray v = Array$ fmap Array v

instance FromJSON Move where
    parseJSON = withObject "move"$ \o -> asum [
        Move <$> o .: "from" <*> o .: "to" <*> o .:? "promote" .!= False,
        Put <$> o .: "to" <*> liftM read (o .: "piece")]
instance ToJSON Move where
    toJSON (Move from to promote) = object ["from".=toJSON from, "to".=toJSON to, "promote".=toJSON promote]
    toJSON (Put to piece) = object ["to".=toJSON to, "piece".=show piece]

instance FromJSON Coord where
    parseJSON = withObject "move"$ \o -> do
        x <- o .: "x"
        y <- o .: "y"
        return$ Coord x y

instance ToJSON Coord where
    toJSON (Coord x y) = object ["x".=x, "y".=y]

instance ToJSON S.Result where
    toJSON = fail "toJSON Result"

instance FromJSON RuleConfig where
    parseJSON text = do
        hm <- parseJSON text
        return$ fromMap hm
instance ToJSON RuleConfig where
    toJSON _ = "\"TODO: rule\""
