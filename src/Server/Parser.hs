{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Server.Parser where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (asum)
import Data.HashMap.Strict as HM (HashMap, toList)
import Data.Map as M (Map, fromList, mapKeys)
import Data.Maybe (fromMaybe)
import Data.Vector as V
  ( Vector(..)
  , (!)
  , cons
  , drop
  , fromList
  , head
  , length
  , singleton
  , take
  , toList
  )
import GHC.Generics (Generic)

import Color (Color(..))
import Coord (Coord(..))
import Board (Board(..), Move(..), AbilityProxy(..), Effector(..), Mover(..), MoverPredicator(..), Slicer(..))
import Board.AbilityProxy
import Board.Const (initialBoard)
import Board.Effector
import Board.Mover
import Board.MoverPredicator
import Board.Slicer
import Hands (Hands(..))
import Piece (Kind, Piece(..), fromJKFKindColor, toJKFKindColor)
import Rule (RuleConfig(..), defaultRuleConfig, fromMap)
import Shogi (Result(..), Shogi(..), Judge(..))
import Shogi.Const (initialShogi)
import Shogi.Judge

data InitialBoardRequest = InitialBoardRequest
  { rule :: RuleConfig } deriving (Show, Generic, FromJSON)

data MoveRequest = MoveRequest
  { shogi :: Shogi
  , move :: Move
  } deriving (Show, Generic, FromJSON)

data Response
  = Response { newShogi :: Shogi
             , next :: Either Shogi.Result [Move] }
  | ErrorResponse { error :: String }
  deriving (Generic, ToJSON, Show)

parseInitialBoardRequest :: ByteString -> Maybe InitialBoardRequest
parseInitialBoardRequest = decode

parseMoveRequest :: ByteString -> Maybe MoveRequest
parseMoveRequest = decode

stringify :: Maybe Response -> ByteString
stringify = encode . fromMaybe (ErrorResponse "cannot stringify")

instance FromJSON Shogi where
  parseJSON =
    withObject "Shogi" $ \o -> do
      turn <- o .: "color"
      board <- o .: "board"
      hands <- o .: "hands"
      ruleConfig <- o .: "rule"
      return $ initialShogi {
        turn = turn,
        board = board {
          getAbilityProxy = abilityProxy ruleConfig,
          getEffector = effector ruleConfig,
          getMover = mover ruleConfig,
          getMoverPredicator = moverPredicator ruleConfig,
          getSlicer = slicer ruleConfig
        },
        hands = hands,
        getJudge = judge ruleConfig
      }

instance ToJSON Shogi where
  toJSON Shogi {hands = hands, board = board, turn = turn, getJudge = getJudge} =
    object
      [
        "board" .= toJSON board,
        "hands" .= toJSON hands,
        "color" .= toJSON turn,
        "rule" .= toJSON RuleConfig {
           abilityProxy = getAbilityProxy board,
           effector = getEffector board,
           mover = getMover board,
           moverPredicator = getMoverPredicator board,
           slicer = getSlicer board,
           judge = getJudge
         }
      ]

instance FromJSON Color where
  parseJSON =
    withScientific "Turn" $ \n ->
      case n of
        0 -> return Black
        1 -> return White
        _ -> fail "Color should be 0 or 1"

instance ToJSON Color where
  toJSON Black = Number 0
  toJSON White = Number 1

instance FromJSON Hands where
  parseJSON =
    withArray "Hands" $ \arr -> do
      black <- parseHand $ arr ! 0
      white <- parseHand $ arr ! 1
      return $ Hands (black, white)
    where
      parseHand text = do
        hm <- parseJSON text
        return $ keyToString hm
      keyToString :: HashMap String Int -> Map Kind Int
      keyToString = M.fromList . map (first read) . HM.toList

-- TODO: Kind should be parsed/stringified correctly. toUpperCase etc
instance ToJSON Hands where
  toJSON (Hands (hand0, hand1)) = toJSON [toJSON hand0', toJSON hand1']
    where
      hand0' = mapKeys show hand0
      hand1' = mapKeys show hand1

instance FromJSON Board where
  parseJSON obj = do
    vector2d <- withArray "array" (mapM parseRow) obj
    let xSize = V.length vector2d
    let ySize = V.length $ V.head vector2d
    let vector = V.fromList $ concatMap V.toList $ V.toList vector2d
    return $ initialBoard {size = (xSize, ySize), vector = vector}
    where
      parseRow = withArray "row" (mapM parseCell)
      parseCell =
        withObject "cell" $ \o -> do
          kind <- o .:? "kind"
          color <- o .:? "color"
          return $ do
            k <- kind
            c <- color
            return $ fromJKFKindColor k c

instance ToJSON Board where
  toJSON Board {size = size, vector = vector} =
    toArray $ unconcat size $ fmap cellToJSON vector
    where
      cellToJSON :: Maybe Piece -> Value
      cellToJSON Nothing = object []
      cellToJSON (Just p) =
        let (kind, color) = toJKFKindColor p
         in object ["kind" .= kind, "color" .= toJSON color]
      unconcat :: (Int, Int) -> Vector Value -> Vector (Vector Value)
      unconcat s@(_, y) v =
        if null next
          then V.singleton $ V.take y v
          else V.take y v `cons` unconcat s next
        where
          next = V.drop y v
      toArray :: Vector (Vector Value) -> Value
      toArray v = Array $ fmap Array v

instance FromJSON Move where
  parseJSON =
    withObject "move" $ \o ->
      asum
        [ Move <$> o .: "from" <*> o .: "to" <*> o .:? "promote" .!= False
        , Put <$> o .: "to" <*> liftM read (o .: "piece")
        ]

instance ToJSON Move where
  toJSON (Move from to promote) =
    object
      ["from" .= toJSON from, "to" .= toJSON to, "promote" .= toJSON promote]
  toJSON (Put to piece) = object ["to" .= toJSON to, "piece" .= show piece]

instance FromJSON Coord where
  parseJSON =
    withObject "move" $ \o -> do
      x <- o .: "x"
      y <- o .: "y"
      return $ Coord x y

instance ToJSON Coord where
  toJSON (Coord x y) = object ["x" .= x, "y" .= y]

instance ToJSON Shogi.Result where
  toJSON = fail "toJSON Result"

instance FromJSON RuleConfig where
  parseJSON =
    withObject "rule" $ \o -> do
      abilityProxy <- parseJSON (Object o)
      effector <- parseJSON (Object o)
      mover <- parseJSON (Object o)
      moverPredicator <- parseJSON (Object o)
      slicer <- parseJSON (Object o)
      judge <- parseJSON (Object o)
      return RuleConfig {
        abilityProxy = abilityProxy,
        effector = effector,
        mover = mover,
        moverPredicator = moverPredicator,
        slicer = slicer,
        judge = judge
      }

instance ToJSON RuleConfig where
  toJSON RuleConfig {
    abilityProxy = abilityProxy,
    effector = effector,
    mover = mover,
    moverPredicator = moverPredicator,
    slicer = slicer,
    judge = judge
  } =
    object
      [
        "AbilityProxy" .= abilityProxyId abilityProxy,
        "Effector" .= effectorId effector,
        "Mover" .= moverId mover,
        "MoverPredicator" .= moverPredicatorId moverPredicator,
        "Slicer" .=slicerId slicer,
        "Judge" .= judgeId judge
      ]

instance FromJSON AbilityProxy where
    parseJSON = withObject "rule" $ \o -> do
       abilityProxyId <- o .: "AbilityProxy"
       return$ getAbilityProxyById abilityProxyId

instance FromJSON Effector where
    parseJSON = withObject "rule" $ \o -> do
       effectorId <- o .: "Effector"
       return$ getEffectorById effectorId

instance FromJSON Mover where
    parseJSON = withObject "rule" $ \o -> do
       moverId <- o .: "Mover"
       return$ getMoverById moverId

instance FromJSON MoverPredicator where
    parseJSON = withObject "rule" $ \o -> do
       moverPredicatorId <- o .: "MoverPredicator"
       return$ getMoverPredicatorById moverPredicatorId

instance FromJSON Slicer where
    parseJSON = withObject "rule" $ \o -> do
       slicerId <- o .: "Slicer"
       return$ getSlicerById slicerId

instance FromJSON Judge where
    parseJSON = withObject "rule" $ \o -> do
       judgeId <- o .: "Judge"
       return$ getJudgeById judgeId