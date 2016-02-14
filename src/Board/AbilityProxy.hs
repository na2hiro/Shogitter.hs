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
import Piece(Piece(..), Kind)
import {-# SOURCE #-} Board
import Control.Monad(mplus)
import Data.Maybe(maybeToList, isJust, catMaybes, mapMaybe)

data NormalAbilityProxy
instance AbilityProxy NormalAbilityProxy where
  abilityProxy _ = normalProxy

data AnnanAbilityProxy
instance AbilityProxy AnnanAbilityProxy where
    abilityProxy color c b = maybeToList$ pieceToKind<$> safeGet back b `mplus` get c b
        where back = addCoord color backward c

data AnhokuAbilityProxy
instance AbilityProxy AnhokuAbilityProxy where
    abilityProxy color c b = maybeToList$ pieceToKind<$> safeGet for b `mplus` get c b
        where for = addCoord color forward c

data AntouzaiAbilityProxy
instance AbilityProxy AntouzaiAbilityProxy where
    abilityProxy color c b = if null lr
        then normalProxy c b
        else map pieceToKind lr
        where lr = mapMaybe (flip safeGet b. (c+)) [left, right]

data AnkiAbilityProxy
instance AbilityProxy AnkiAbilityProxy where
    abilityProxy color c b = if null p8
        then normalProxy c b
        else  map pieceToKind p8
        where happou = [c*unit|c<-[Coord 1 2, Coord 2 1], unit<-[Coord 1 1, Coord 1 (-1), Coord (-1) 1, Coord (-1) (-1)]]
              p8 = mapMaybe (flip safeGet b. flip (addCoord color) c) happou

data TenjikuAbilityProxy
instance AbilityProxy TenjikuAbilityProxy where
    abilityProxy color c b = map pieceToKind$ catMaybes [safeGet back b, get c b]
        where back = addCoord color backward c

sliceWhile :: Coord -> Coord -> (Cell -> Bool) -> Board a s -> [Cell]
sliceWhile base vec cond board@(Board _) = takeWhile cond$ map (`get` board)$ slice base vec board

data NekosenAbilityProxy
instance AbilityProxy NekosenAbilityProxy where
    abilityProxy color c b = [pieceToKind$ unsafeGet (Coord (getX c) (getY c+backwards-forwards)) b]
        where forwards = length$ sliceWhile c forward cond b
              backwards = length$ sliceWhile c backward cond b
              cond (Just (Piece color' _ _)) | color==color' = True
              cond _ = False

data YokoNekosenAbilityProxy
instance AbilityProxy YokoNekosenAbilityProxy where
    abilityProxy color c b = [pieceToKind$ unsafeGet (Coord (getX c+lefts-rights) (getY c)) b]
        where rights = length$ sliceWhile c right cond b
              lefts = length$ sliceWhile c left cond b
              cond (Just (Piece color' _ _)) | color==color' = True
              cond _ = False

data NekonekosenAbilityProxy
instance AbilityProxy NekonekosenAbilityProxy where
    abilityProxy color c b = [pieceToKind$ unsafeGet (Coord (getX c) (getY c+backwards-forwards)) b]
        where forwards = length$ sliceWhile c forward isJust b
              backwards = length$ sliceWhile c backward isJust b

data YokoNekonekosenAbilityProxy
instance AbilityProxy YokoNekonekosenAbilityProxy where
    abilityProxy color c b = [pieceToKind$ unsafeGet (Coord (getX c+lefts-rights) (getY c)) b]
        where rights = length$ sliceWhile c right isJust b
              lefts = length$ sliceWhile c left isJust b

data TaimenAbilityProxy
instance AbilityProxy TaimenAbilityProxy where
    abilityProxy color c b = case safeGet (addCoord color forward c) b of
        Just (Piece color' _ kind) | color/=color' -> [kind]
        _ -> normalProxy c b

data HaimenAbilityProxy
instance AbilityProxy HaimenAbilityProxy where
    abilityProxy color c b = case safeGet (addCoord color backward c) b of
        Just (Piece color' _ kind) | color/=color' -> [kind]
        _ -> normalProxy c b

normalProxy :: Coord -> Board a s -> [Kind]
normalProxy c b =return$ pieceToKind$ unsafeGet c b

pieceToKind (Piece _ _ kind) = kind
