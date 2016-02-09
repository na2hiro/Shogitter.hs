module Hands(Hands, kindsHand, addToHands, removeFromHands, initialHands) where

import Piece(Color(..), Kind)
import Data.Map as M(Map, lookup, insert, delete, empty, foldrWithKey, keys)
import Control.Arrow((***))
import Control.Monad(liftM, liftM2)
import Data.Functor.Identity

type Hand = Map Kind Int
data Hands = Hands (Hand, Hand) deriving (Eq)
instance Show Hands where
    show (Hands (b, w)) = showHand Black b ++ showHand White w
        where showHand c hand = "P" ++ show c ++ foldrWithKey (\kind num str->str++(concat. replicate num$ "00"++show kind)) "" hand ++ "\n"

initialHands = Hands (empty, empty)

kindsHand :: Color -> Hands -> [Kind]
kindsHand color = keys. getHand color

getHand :: Color -> Hands -> Hand
getHand Black (Hands t) = fst t
getHand _ (Hands t) = snd t

applyHand :: Color -> (Hand -> Hand) -> Hands -> Hands
applyHand color f = runIdentity. applyHandM color (Identity. f)

applyHandM :: Monad m => Color -> (Hand -> m Hand) -> Hands -> m Hands
applyHandM Black f (Hands t) = liftM Hands$ liftTuple$ (f *** return) t
applyHandM White f (Hands t) = liftM Hands$ liftTuple$ (return *** f) t

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
