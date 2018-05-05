module Server.Server where

import Data.ByteString.Lazy (ByteString)

import Rule (RuleConfig(..))
import Server.Parser (InitialBoardRequest(..), MoveRequest(..), Response(..), parseInitialBoardRequest, parseMoveRequest, stringify)
import Shogi as S (Shogi, board, doMove, getMovesShogi, judge)
import Shogi.Const (initialShogi)


-- Process request for initial board

serveInitialBoard :: ByteString -> ByteString
serveInitialBoard json = stringify $ processInitialBoardRequest =<< parseInitialBoardRequest json

processInitialBoardRequest :: InitialBoardRequest -> Maybe Response
processInitialBoardRequest (InitialBoardRequest ruleConfig) = do
  return $ Response initialShogi (Right $ getMovesShogi initialShogi)


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
