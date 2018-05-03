module Server.Server where

import Data.ByteString.Lazy(ByteString)

import Shogi as S(Shogi, doMove, judge, getMovesShogi, board)
import Server.Parser(stringify, parse, Request(..), Response(..))
import Rule(RuleConfig(..), injectRule)

serve :: ByteString -> ByteString
serve json = stringify$ process =<< parse json

process :: Request -> Maybe Response
process (Request rule shogi move) = do
    let shogi' = injectRule rule shogi
    nextShogi <- doMove move shogi'
    let movesOrJudgment = case S.judge nextShogi of
            Nothing -> Right$ getMovesShogi nextShogi
            Just r -> Left r
    return$ Response nextShogi movesOrJudgment
