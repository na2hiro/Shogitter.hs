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
  , abilityProxies
  ) where

import Board
import Color (Color)
import Coord (Coord(..), getX, getY)
import Coord.Const (backward, forward, left, right)
import Data.Maybe (isJust, mapMaybe)
import Piece (Kind, Piece(..), Promoted)

abilityProxies :: [AbilityProxy]
abilityProxies =
  [ normalAbilityProxy
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
  ]

normalAbilityProxy =
  AbilityProxy {abilityProxyId = "normal", runAbilityProxy = abilityProxy}
  where
    abilityProxy _ = normalProxy

annanAbilityProxy =
  AbilityProxy {abilityProxyId = "annan", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      case safeGet b $addCoord color c backward of
        Just (Piece color' promoted kind)
          | color == color' -> [(promoted, kind)]
        _ -> return $ pieceToPromotedKind $ unsafeGet b c

anhokuAbilityProxy =
  AbilityProxy {abilityProxyId = "anhoku", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      case safeGet b $ addCoord color c forward of
        Just (Piece color' promoted kind)
          | color == color' -> [(promoted, kind)]
        _ -> return $ pieceToPromotedKind $ unsafeGet b c

antouzaiAbilityProxy =
  AbilityProxy {abilityProxyId = "antouzai", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      if null lr
        then normalProxy c b
        else map pieceToPromotedKind lr
      where
        lr = filter (isColor color) $ mapMaybe (safeGet b . (c +)) [left, right]

isColor :: Color -> Piece -> Bool
isColor c (Piece c' _ _)
  | c == c' = True
isColor _ _ = False

ankiAbilityProxy =
  AbilityProxy {abilityProxyId = "anki", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      if null p8
        then normalProxy c b
        else map pieceToPromotedKind p8
      where
        happou =
          [ c * unit
          | c <- [Coord 1 2, Coord 2 1]
          , unit <- [Coord 1 1, Coord 1 (-1), Coord (-1) 1, Coord (-1) (-1)]
          ]
        p8 =
          filter (isColor color) $
          mapMaybe (safeGet b . addCoord color c) happou

tenjikuAbilityProxy =
  AbilityProxy {abilityProxyId = "tenjiku", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      case safeGet b $ addCoord color c backward of
        Just (Piece color' promoted kind)
          | color == color' ->
            [(promoted, kind), pieceToPromotedKind $ unsafeGet b c]
        _ -> return $ pieceToPromotedKind $ unsafeGet b c

sliceWhile :: Coord -> Coord -> (Cell -> Bool) -> Board -> [Cell]
sliceWhile base vec cond board = takeWhile cond $ map snd $ slice board base vec

nekosenAbilityProxy =
  AbilityProxy {abilityProxyId = "nekosen", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      [ pieceToPromotedKind $
        unsafeGet b $ Coord (getX c) (getY c + backwards - forwards)
      ]
      where
        forwards = length $ sliceWhile c forward cond b
        backwards = length $ sliceWhile c backward cond b
        cond (Just (Piece color' _ _))
          | color == color' = True
        cond _ = False

yokoNekosenAbilityProxy =
  AbilityProxy {abilityProxyId = "yokoNekosen", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      [ pieceToPromotedKind $
        unsafeGet b $ Coord (getX c + lefts - rights) (getY c)
      ]
      where
        rights = length $ sliceWhile c right cond b
        lefts = length $ sliceWhile c left cond b
        cond (Just (Piece color' _ _))
          | color == color' = True
        cond _ = False

nekonekosenAbilityProxy =
  AbilityProxy {abilityProxyId = "nekonekosen", runAbilityProxy = abilityProxy}
  where
    abilityProxy _ c b =
      [ pieceToPromotedKind $
        unsafeGet b $ Coord (getX c) (getY c + backwards - forwards)
      ]
      where
        forwards = length $ sliceWhile c forward isJust b
        backwards = length $ sliceWhile c backward isJust b

yokoNekonekosenAbilityProxy =
  AbilityProxy
    {abilityProxyId = "yokoNekonekosen", runAbilityProxy = abilityProxy}
  where
    abilityProxy _ c b =
      [ pieceToPromotedKind $
        unsafeGet b $ Coord (getX c + lefts - rights) (getY c)
      ]
      where
        rights = length $ sliceWhile c right isJust b
        lefts = length $ sliceWhile c left isJust b

taimenAbilityProxy =
  AbilityProxy {abilityProxyId = "taimen", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      case safeGet b $ addCoord color c forward of
        Just (Piece color' promoted kind)
          | color /= color' -> [(promoted, kind)]
        _ -> normalProxy c b

haimenAbilityProxy =
  AbilityProxy {abilityProxyId = "haimen", runAbilityProxy = abilityProxy}
  where
    abilityProxy color c b =
      case safeGet b $ addCoord color c backward of
        Just (Piece color' promoted kind)
          | color /= color' -> [(promoted, kind)]
        _ -> normalProxy c b

normalProxy :: Coord -> Board -> [(Promoted, Kind)]
normalProxy c b = return $ pieceToPromotedKind $ unsafeGet b c

pieceToPromotedKind :: Piece -> (Promoted, Kind)
pieceToPromotedKind (Piece _ promoted kind) = (promoted, kind)
