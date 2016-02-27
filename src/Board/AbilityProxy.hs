module Board.AbilityProxy
    ( NormalAbilityProxy
    , AnnanAbilityProxy
    , AnhokuAbilityProxy
    , AntouzaiAbilityProxy
    , AnkiAbilityProxy
    , TenjikuAbilityProxy
    , NekosenAbilityProxy
    , YokoNekosenAbilityProxy
    , NekonekosenAbilityProxy
    , YokoNekonekosenAbilityProxy
    , TaimenAbilityProxy
    , HaimenAbilityProxy
    ) where

import Coord(Coord(..), getX, getY)
import Coord.Const(forward, backward, right, left)
import Color(Color)
import Piece(Piece(..), Kind, Promoted)
import {-# SOURCE #-} Board
import Control.Monad(mplus)
import Data.Maybe(maybeToList, isJust, catMaybes, mapMaybe)

data NormalAbilityProxy
instance AbilityProxy NormalAbilityProxy where
  abilityProxy _ = normalProxy

data AnnanAbilityProxy
instance AbilityProxy AnnanAbilityProxy where
    abilityProxy color c b = case safeGet b $addCoord color c backward of
        Just (Piece color' promoted kind) | color==color' -> [(promoted, kind)]
        _ -> return$ pieceToPromotedKind$ unsafeGet b c

data AnhokuAbilityProxy
instance AbilityProxy AnhokuAbilityProxy where
    abilityProxy color c b = case safeGet b$ addCoord color c forward of
        Just (Piece color' promoted kind) | color==color' -> [(promoted, kind)]
        _ -> return$ pieceToPromotedKind$ unsafeGet b c

data AntouzaiAbilityProxy
instance AbilityProxy AntouzaiAbilityProxy where
    abilityProxy color c b = if null lr
        then normalProxy c b
        else map pieceToPromotedKind lr
        where lr = filter (isColor color)$ mapMaybe (safeGet b. (c+)) [left, right]

isColor :: Color -> Piece -> Bool
isColor c (Piece c' _ _) | c==c' = True
isColor _ _ = False

data AnkiAbilityProxy
instance AbilityProxy AnkiAbilityProxy where
    abilityProxy color c b = if null p8
        then normalProxy c b
        else  map pieceToPromotedKind p8
        where happou = [c*unit|c<-[Coord 1 2, Coord 2 1], unit<-[Coord 1 1, Coord 1 (-1), Coord (-1) 1, Coord (-1) (-1)]]
              p8 = filter (isColor color)$ mapMaybe (safeGet b. addCoord color c) happou

data TenjikuAbilityProxy
instance AbilityProxy TenjikuAbilityProxy where
    abilityProxy color c b = case safeGet b$ addCoord color c backward of
        Just (Piece color' promoted kind) | color==color' -> [(promoted, kind), pieceToPromotedKind$ unsafeGet b c]
        _ -> return$ pieceToPromotedKind$ unsafeGet b c

sliceWhile :: Coord -> Coord -> (Cell -> Bool) -> Board m e a s -> [Cell]
sliceWhile base vec cond board@(Board _ _) = takeWhile cond$ map snd$ slice board base vec

data NekosenAbilityProxy
instance AbilityProxy NekosenAbilityProxy where
    abilityProxy color c b = [pieceToPromotedKind$ unsafeGet b$ Coord (getX c) (getY c+backwards-forwards)]
        where forwards = length$ sliceWhile c forward cond b
              backwards = length$ sliceWhile c backward cond b
              cond (Just (Piece color' _ _)) | color==color' = True
              cond _ = False

data YokoNekosenAbilityProxy
instance AbilityProxy YokoNekosenAbilityProxy where
    abilityProxy color c b = [pieceToPromotedKind$ unsafeGet b$ Coord (getX c+lefts-rights) (getY c)]
        where rights = length$ sliceWhile c right cond b
              lefts = length$ sliceWhile c left cond b
              cond (Just (Piece color' _ _)) | color==color' = True
              cond _ = False

data NekonekosenAbilityProxy
instance AbilityProxy NekonekosenAbilityProxy where
    abilityProxy color c b = [pieceToPromotedKind$ unsafeGet b$ Coord (getX c) (getY c+backwards-forwards)]
        where forwards = length$ sliceWhile c forward isJust b
              backwards = length$ sliceWhile c backward isJust b

data YokoNekonekosenAbilityProxy
instance AbilityProxy YokoNekonekosenAbilityProxy where
    abilityProxy color c b = [pieceToPromotedKind$ unsafeGet b$ Coord (getX c+lefts-rights) (getY c)]
        where rights = length$ sliceWhile c right isJust b
              lefts = length$ sliceWhile c left isJust b

data TaimenAbilityProxy
instance AbilityProxy TaimenAbilityProxy where
    abilityProxy color c b = case safeGet b$ addCoord color c forward of
        Just (Piece color' promoted kind) | color/=color' -> [(promoted, kind)]
        _ -> normalProxy c b

data HaimenAbilityProxy
instance AbilityProxy HaimenAbilityProxy where
    abilityProxy color c b = case safeGet b$ addCoord color c backward of
        Just (Piece color' promoted kind) | color/=color' -> [(promoted, kind)]
        _ -> normalProxy c b

normalProxy :: Coord -> Board m e a s -> [(Promoted, Kind)]
normalProxy c b =return$ pieceToPromotedKind$ unsafeGet b c

pieceToPromotedKind :: Piece -> (Promoted, Kind)
pieceToPromotedKind (Piece _ promoted kind) = (promoted, kind)
