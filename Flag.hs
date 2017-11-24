{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flag
    ( showFlag
    ) where

import Data.Map (fromList)
import Reflex
import Reflex.Dom

import Pos
import Svg

showFlag :: MonadWidget t m => Pos -> m [El t]
showFlag pos = do
    let flagAttrs =
            fromList
                [ ("points", "0.20,0.40 0.70,0.55 0.70,0.25")
                , ("style", "fill:red")
                ]
    (fEl, _) <- elSvgns "polygon" (constDyn flagAttrs) $ return ()
    let poleAttrs =
            fromList
                [ ("x1", "0.70")
                , ("y1", "0.25")
                , ("x2", "0.70")
                , ("y2", "0.85")
                , ("stroke-width", ".07")
                , ("stroke", "black")
                ]
    (pEl, _) <- elSvgns "line" (constDyn poleAttrs) $ return ()
    return [fEl, pEl]
