module Server.Server where

import Data.ByteString.Lazy (ByteString)

import Board (Board(..), Move(..), AbilityProxy(..), Effector(..), Mover(..), MoverPredicator(..), Slicer(..))
import Board.Const (initialBoard)
import Rule (RuleConfig(..))
import Server.Parser (InitialBoardRequest(..), MoveRequest(..), Response(..), parseInitialBoardRequest, parseMoveRequest, stringify)
import Shogi as S (Shogi(..), Judge(..), board, doMove, getMovesShogi, judge)
import Shogi.Const (initialShogi)


-- Process request for initial board

serveInitialBoard :: ByteString -> ByteString
serveInitialBoard json = stringify $ processInitialBoardRequest =<< parseInitialBoardRequest json

processInitialBoardRequest :: InitialBoardRequest -> Maybe Response
processInitialBoardRequest (InitialBoardRequest ruleConfig) = do
  return $ Response shogi (Right $ getMovesShogi shogi)
  where shogi = initialShogi {
    board = initialBoard {
      getAbilityProxy = abilityProxy ruleConfig,
      getEffector = effector ruleConfig,
      getMover = mover ruleConfig,
      getMoverPredicator = moverPredicator ruleConfig,
      getSlicer = slicer ruleConfig
    },
    getJudge = Rule.judge ruleConfig
  }

-- Process request for move

serveMove :: ByteString -> ByteString
serveMove json = stringify $ processMove =<< parseMoveRequest json

processMove :: MoveRequest -> Maybe Response
processMove (MoveRequest shogi move) = do
  nextShogi <- doMove move shogi
  let movesOrJudgment =
        case S.judge nextShogi of
          Nothing -> Right $ getMovesShogi nextShogi
          Just r -> Left r
  return $ Response nextShogi movesOrJudgment
