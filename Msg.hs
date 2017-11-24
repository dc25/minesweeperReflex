module Msg where

import Pos

data Msg
    = LeftPick Pos
    | RightPick Pos
