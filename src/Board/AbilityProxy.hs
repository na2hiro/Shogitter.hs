module Board.AbilityProxy
    ( normalAbilityProxy
    , annanAbilityProxy
    , anhokuAbilityProxy
    , antouzaiAbilityProxy
    , ankiAbilityProxy
    , tenjikuAbilityProxy
    , nekosenAbilityProxy
    , yokoNekosenAbilityProxy
    , nekonekosenAbilityProxy
    , yokoNekonekosenAbilityProxy
    , taimenAbilityProxy
    , haimenAbilityProxy
    ) where

import Coord(Coord(..), getX, getY)
import Coord.Const(forward, backward, right, left)
import Color(Color)
import Piece(Piece(..), Kind, Promoted)
import Board
import Data.Maybe(isJust, mapMaybe)

normalAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy _ = normalProxy

annanAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = case safeGet b $addCoord color c backward of
        Just (Piece color' promoted kind) | color==color' -> [(promoted, kind)]
        _ -> return$ pieceToPromotedKind$ unsafeGet b c

anhokuAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = case safeGet b$ addCoord color c forward of
        Just (Piece color' promoted kind) | color==color' -> [(promoted, kind)]
        _ -> return$ pieceToPromotedKind$ unsafeGet b c

antouzaiAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = if null lr
        then normalProxy c b
        else map pieceToPromotedKind lr
        where lr = filter (isColor color)$ mapMaybe (safeGet b. (c+)) [left, right]

isColor :: Color -> Piece -> Bool
isColor c (Piece c' _ _) | c==c' = True
isColor _ _ = False

ankiAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = if null p8
        then normalProxy c b
        else  map pieceToPromotedKind p8
        where happou = [c*unit|c<-[Coord 1 2, Coord 2 1], unit<-[Coord 1 1, Coord 1 (-1), Coord (-1) 1, Coord (-1) (-1)]]
              p8 = filter (isColor color)$ mapMaybe (safeGet b. addCoord color c) happou

tenjikuAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = case safeGet b$ addCoord color c backward of
        Just (Piece color' promoted kind) | color==color' -> [(promoted, kind), pieceToPromotedKind$ unsafeGet b c]
        _ -> return$ pieceToPromotedKind$ unsafeGet b c

sliceWhile :: Coord -> Coord -> (Cell -> Bool) -> Board -> [Cell]
sliceWhile base vec cond board = takeWhile cond$ map snd$ slice board base vec

nekosenAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = [pieceToPromotedKind$ unsafeGet b$ Coord (getX c) (getY c+backwards-forwards)]
        where forwards = length$ sliceWhile c forward cond b
              backwards = length$ sliceWhile c backward cond b
              cond (Just (Piece color' _ _)) | color==color' = True
              cond _ = False

yokoNekosenAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = [pieceToPromotedKind$ unsafeGet b$ Coord (getX c+lefts-rights) (getY c)]
        where rights = length$ sliceWhile c right cond b
              lefts = length$ sliceWhile c left cond b
              cond (Just (Piece color' _ _)) | color==color' = True
              cond _ = False

nekonekosenAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy _ c b = [pieceToPromotedKind$ unsafeGet b$ Coord (getX c) (getY c+backwards-forwards)]
        where forwards = length$ sliceWhile c forward isJust b
              backwards = length$ sliceWhile c backward isJust b

yokoNekonekosenAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy _ c b = [pieceToPromotedKind$ unsafeGet b$ Coord (getX c+lefts-rights) (getY c)]
        where rights = length$ sliceWhile c right isJust b
              lefts = length$ sliceWhile c left isJust b

taimenAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = case safeGet b$ addCoord color c forward of
        Just (Piece color' promoted kind) | color/=color' -> [(promoted, kind)]
        _ -> normalProxy c b

haimenAbilityProxy = AbilityProxy {
    runAbilityProxy = abilityProxy
} where
    abilityProxy color c b = case safeGet b$ addCoord color c backward of
        Just (Piece color' promoted kind) | color/=color' -> [(promoted, kind)]
        _ -> normalProxy c b

normalProxy :: Coord -> Board -> [(Promoted, Kind)]
normalProxy c b =return$ pieceToPromotedKind$ unsafeGet b c

pieceToPromotedKind :: Piece -> (Promoted, Kind)
pieceToPromotedKind (Piece _ promoted kind) = (promoted, kind)
