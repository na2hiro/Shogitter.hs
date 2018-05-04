module Server.Server where

import Data.ByteString.Lazy (ByteString)

import Rule (RuleConfig(..))
import Server.Parser (Request(..), Response(..), parse, stringify)
import Shogi as S (Shogi, board, doMove, getMovesShogi, judge)

serve :: ByteString -> ByteString
serve json = stringify $ process =<< parse json

process :: Request -> Maybe Response
process (Request shogi move) = do
  nextShogi <- doMove move shogi
  let movesOrJudgment =
        case S.judge nextShogi of
          Nothing -> Right $ getMovesShogi nextShogi
          Just r -> Left r
  return $ Response nextShogi movesOrJudgment
