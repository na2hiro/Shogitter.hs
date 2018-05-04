module Rule where

import Data.List (find)
import Data.Map as M (Map, lookup)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import Board
  ( AbilityProxy
  , Board(..)
  , Effector
  , Mover
  , MoverPredicator
  , Slicer
  , abilityProxyId
  , effectorId
  , moverId
  , moverPredicatorId
  , slicerId
  )
import Board.AbilityProxy
import Board.Effector
import Board.Mover
import Board.MoverPredicator
import Board.Slicer
import Shogi (Judge, Shogi(..), judgeId)
import Shogi.Judge

class Rule r where
  ruleId :: r -> String
  ruleInstanceId :: r -> String
  defaultRule :: r
  ruleInstances :: [r]

data RuleConfig = RuleConfig
  { abilityProxy :: AbilityProxy
  , effector :: Effector
  , mover :: Mover
  , moverPredicator :: MoverPredicator
  , slicer :: Slicer
  , judge :: Judge
  } deriving Eq

instance Show RuleConfig where
  show ruleConfig =
    "RuleConfig{" ++
    showRule (abilityProxy ruleConfig) ++
    showRule (effector ruleConfig) ++
    showRule (mover ruleConfig) ++
    showRule (moverPredicator ruleConfig) ++
    showRule (slicer ruleConfig) ++ showRule (judge ruleConfig) ++ "}"
    where
      showRule rule =
        "  " ++ ruleId rule ++ ": " ++ ruleInstanceId rule ++ ",\n"

injectRule :: RuleConfig -> Shogi -> Shogi
injectRule conf shogi = shogi {getJudge = Rule.judge conf, board = board'}
  where
    board' =
      (board shogi)
        { getMover = mover conf
        , getEffector = effector conf
        , getAbilityProxy = abilityProxy conf
        , getSlicer = slicer conf
        , getMoverPredicator = moverPredicator conf
        }

fromMap :: Map String String -> RuleConfig
fromMap map =
  RuleConfig
    { abilityProxy = getRule map
    , effector = getRule map
    , mover = getRule map
    , moverPredicator = getRule map
    , slicer = getRule map
    , judge = getRule map
    }

getRule :: Rule r => Map String String -> r
getRule map = fromMaybe def rule
  where
    ruleCat = ruleId def
    rule :: Rule r => Maybe r
    rule = do
      id <- M.lookup ruleCat map
      find (\rule -> ruleInstanceId rule == id) ruleInstances
    def = defaultRule

defaultRuleConfig =
  RuleConfig
    { abilityProxy = defaultRule
    , effector = defaultRule
    , mover = defaultRule
    , moverPredicator = defaultRule
    , slicer = defaultRule
    , judge = defaultRule
    }

instance Rule Judge where
  ruleId _ = "Judge"
  ruleInstanceId = judgeId
  defaultRule = normalJudge
  ruleInstances = judges

instance Rule AbilityProxy where
  ruleId _ = "AbilityProxy"
  ruleInstanceId = abilityProxyId
  defaultRule = normalAbilityProxy
  ruleInstances = abilityProxies

instance Rule Effector where
  ruleId _ = "Effector"
  ruleInstanceId = effectorId
  defaultRule = normalEffector
  ruleInstances = effectors

instance Rule Mover where
  ruleId _ = "Mover"
  ruleInstanceId = moverId
  defaultRule = normalMover
  ruleInstances = movers

instance Rule MoverPredicator where
  ruleId _ = "MoverPredicator"
  ruleInstanceId = moverPredicatorId
  defaultRule = normalMoverPredicator
  ruleInstances = moverPredicators

instance Rule Slicer where
  ruleId _ = "Slicer"
  ruleInstanceId = slicerId
  defaultRule = normalSlicer
  ruleInstances = slicers
