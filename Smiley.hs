{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Smiley
    ( showFace
    ) where

import Data.Foldable (forM_)
import Data.Map (fromList)
import Data.Text (pack)
import Reflex
import Reflex.Dom

import Svg

showFace :: MonadWidget t m => Bool -> m ()
showFace lost = do
    let sz = 100 :: Int
    svgEl "svg" (constDyn $ fromList [("width", "100"), ("height", "100")]) $
        svgEl
            "g"
            (constDyn $
             fromList
                 [ ( "transform"
                   , pack $
                     "scale (" ++
                     show sz ++
                     ", " ++ show sz ++ ") " ++ "translate (0.5, 0.5)")
                 ]) $
                -- face outline
         do
            svgEl
                "circle"
                (constDyn $
                 fromList
                     [ ("cx", "0.0")
                     , ("cy", "0.0")
                     , ("r", "0.4")
                     , ("style", "fill:yellow")
                     , ("stroke", "black")
                     , ("stroke-width", "0.02")
                     ]) $
                return ()
                -- right eye
            svgEl
                "circle"
                (constDyn $
                 fromList
                     [ ("cx", "0.15")
                     , ("cy", "-0.1")
                     , ("r", "0.08")
                     , ("style", "fill:yellow")
                     , ("stroke", "black")
                     , ("stroke-width", "0.02")
                     ]) $
                return ()
                -- left eye
            svgEl
                "circle"
                (constDyn $
                 fromList
                     [ ("cx", "-0.15")
                     , ("cy", "-0.1")
                     , ("r", "0.08")
                     , ("style", "fill:yellow")
                     , ("stroke", "black")
                     , ("stroke-width", "0.02")
                     ]) $
                return ()
            if lost
                    -- eye crosses
                then (forM_
                          [ (ex, dx, dy) :: (Float, Float, Float)
                          | ex <- [-0.15, 0.15]
                          , dx <- [-0.1, 0.1]
                          , dy <- [-0.1, 0.1]
                          ]
                          (\(ex, px, py) ->
                               svgEl
                                   "path"
                                   (constDyn $
                                    fromList
                                        [ ( "d"
                                          , pack $
                                            "M " ++
                                            show ex ++
                                            " -0.1 l " ++
                                            show px ++ " " ++ show py)
                                        , ("stroke", "black")
                                        , ("stroke-width", "0.02")
                                        , ("fill", "none")
                                        ]) $
                               return ()))
                    -- right eyeball
                else do
                    svgEl
                        "circle"
                        (constDyn $
                         fromList
                             [ ("cx", "0.15")
                             , ("cy", "-0.1")
                             , ("r", "0.04")
                             , ("style", "fill:black")
                             ]) $
                        return ()
                    -- left eyeball
                    svgEl
                        "circle"
                        (constDyn $
                         fromList
                             [ ("cx", "-0.15")
                             , ("cy", "-0.1")
                             , ("r", "0.04")
                             , ("style", "fill:black")
                             ]) $
                        return ()
                -- smile/frown
            svgEl
                "path"
                (constDyn $
                 fromList
                     [ ( "d"
                       , pack $
                         "M-0.15,0.15 a0.2,0.2 0 0 " ++
                         (if lost
                              then "1"
                              else "0") ++
                         " 0.30,0.0")
                     , ("stroke", "black")
                     , ("stroke-width", "0.02")
                     , ("fill", "none")
                     ]) $
                return ()
