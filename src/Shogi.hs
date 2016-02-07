module Shogi where

import Board
import Piece
import Data.Map as M(Map, lookup, insert, delete, empty, foldrWithKey, keys)
import Control.Arrow((***))
import Control.Monad(guard, liftM2)
import Data.Array(indices)
import Data.Functor.Identity

type Hand = Map Kind Int
type Turn = Color
type Hands = (Hand, Hand)

data Shogi = Shogi Turn Board Hands deriving (Eq)
instance Show Shogi where
    show (Shogi turn board hands) = show board ++ showHands hands ++ show turn ++ "\n"
        where showHands (b,w) = showHand Black b ++ showHand White w
              showHand c hand = "P" ++ show c ++ foldrWithKey (\kind num str->str++(concat. replicate num$ "00"++show kind)) "" hand ++ "\n"

kindsHand :: Color -> Hands -> [Kind]
kindsHand color = keys. getHand color

getHand :: Color -> Hands -> Hand
getHand Black = fst
getHand _ = snd

initialShogi :: Shogi
initialShogi = Shogi Black initialBoard (empty, empty)

applyHand :: Color -> (Hand -> Hand) -> Hands -> Hands
applyHand color f = runIdentity. applyHandM color (Identity. f)

applyHandM :: Monad m => Color -> (Hand -> m Hand) -> Hands -> m Hands
applyHandM Black f = liftTuple. (f *** return)
applyHandM White f = liftTuple. (return *** f)

liftTuple :: Monad m => (m a, m b) -> m (a, b)
liftTuple = uncurry$ liftM2 (,)

addToHand :: Kind -> Hand -> Hand
addToHand kind hand = case M.lookup kind hand of
                        Nothing -> insert kind 1 hand
                        Just n -> insert kind (n+1) hand

removeFromHand :: Kind -> Hand -> Maybe Hand
removeFromHand kind hand = do
    n <- M.lookup kind hand
    return$ if n==1
        then delete kind hand
        else insert kind (n-1) hand

addToHands :: Color -> Kind -> Hands -> Hands
addToHands color = applyHand color. addToHand

removeFromHands :: Color -> Kind -> Hands -> Maybe Hands
removeFromHands color = applyHandM color. removeFromHand

getMoves :: Shogi -> [Move]
getMoves (Shogi turn board@(Board arr) hands) = do
    from <- indices arr
    case get from board of
        Nothing -> map (Put from) kinds
        Just (Piece color _ _) -> do
            guard$ color == turn
            dest <- destinationsAt from board
            if canPromote color board from || canPromote color board dest
                then map (Move from dest) [True,False]
                else return$ Move from dest False
    where kinds = kindsHand turn hands

getNext :: Shogi -> [Shogi]
getNext board = [unsafeDoMove move board | move <- getMoves board]

unsafeDoMove :: Move -> Shogi -> Shogi
unsafeDoMove (Move from to promoted) (Shogi turn board hands) = Shogi turn' board' hands'
    where Just fromPiece =  get from board
          fromPiece' = Just$ promote promoted fromPiece
          turn' = opposite turn
          board' = sets [(from, Nothing), (to, fromPiece')] board
          hands' = case get to board of
            Nothing -> hands
            Just (Piece _ _ kind) -> addToHands turn kind hands
unsafeDoMove (Put to kind) (Shogi turn board hands) = Shogi (opposite turn) board' hands'
    where Just hands' = removeFromHands turn kind hands
          board' = set (to, Just$ Piece turn False kind) board

doMove :: Move -> Shogi -> Maybe Shogi
doMove (Move from to promoted) (Shogi turn board hands) = do
    guard$ board `inRange` from
    guard$ board `inRange` to
    fromPiece@(Piece color _ _) <- get from board
    guard$ turn==color
    hands' <- case get to board of
        Nothing -> return hands
        Just (Piece color _ kind) -> if color==turn
            then Nothing
            else Just$ addToHands turn kind hands
    let fromPiece' = Just$ promote promoted fromPiece
    let turn' = opposite turn
    let board' = sets [(from, Nothing), (to, fromPiece')] board
    return$ Shogi turn' board' hands'
doMove (Put to kind) (Shogi turn board hands) = do
    guard$ board `inRange` to
    hands' <- removeFromHands turn kind hands
    case get to board of
        Nothing -> return$ Shogi (opposite turn) board' hands'
        Just _ -> Nothing
    where board' = set (to, Just$ Piece turn False kind) board

