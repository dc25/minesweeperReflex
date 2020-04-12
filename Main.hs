{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

import Control.Monad.Random (RandomGen, getStdGen, runRand, split)
import Control.Monad.State (runState)
import Data.List (unfoldr)
import Data.Map (Map, elems, fromList)
import Data.Text (Text, pack)
import Reflex
import Reflex.Dom

import Board
import Flag
import Mine
import Msg
import Pos
import Smiley
import Svg

cellSize :: Int
cellSize = 20

getColor :: Cell -> String
getColor (Cell _ exposed _ _) =
    if exposed
        then "#909090"
        else "#CCCCCC"

squareAttrs :: Cell -> Map Text Text
squareAttrs cell =
    fromList
        [ ("x", "0.05")
        , ("y", "0.05")
        , ("width", "0.9")
        , ("height", "0.9")
        , ("style", pack $ "fill:" ++ getColor cell)
        ]

showSquare :: MonadWidget t m => Cell -> m [El t]
showSquare cell = do
    (rEl, _) <- elSvgns "rect" (constDyn $ squareAttrs cell) $ return ()
    return [rEl]

textAttrs :: Int -> Map Text Text
textAttrs count =
    let textColor :: Text =
            case count of
                1 -> "blue"
                2 -> "green"
                3 -> "red"
                4 -> "brown"
                _ -> "purple"
    in fromList
           [ ("x", "0.5")
           , ("y", "0.87")
           , ("font-size", "1.0")
           , ("fill", textColor)
           , ("text-anchor", "middle")
           ]

showText :: MonadWidget t m => Int -> m [El t]
showText count = do
    elSvgns "text" (constDyn $ textAttrs count) $ text $ pack $ show count
    return []

showCellDetail :: MonadWidget t m => Pos -> Cell -> m [El t]
showCellDetail pos (Cell mined exposed flagged mineCount) =
    case ( flagged,    mined, exposed, 0 /= mineCount) of
         (    True,       _,       _,       _) -> showFlag pos
         (       _,    True,    True,       _) -> showMine pos
         (       _,       _,    True,    True) -> showText mineCount
         (       _,       _,       _,       _) -> return []

mouseEv :: Reflex t => Pos -> El t -> [Event t Msg]
mouseEv pos el =
    let r_rEv = RightPick pos <$ domEvent Contextmenu el
        l_rEv = LeftPick pos <$ domEvent Click el
    in [l_rEv, r_rEv]

groupAttrs :: Pos -> Map Text Text
groupAttrs (x, y) =
    let scale = show cellSize
    in fromList
           [ ( "transform" 
             , pack $    "scale (" ++ scale ++ ", " ++ scale ++ ") " 
                      ++ "translate (" ++ show x ++ ", " ++ show y ++ ")")
           ]

showCell :: MonadWidget t m => Pos -> Cell -> m (Event t Msg)
showCell pos cell =
    fmap snd $
    elSvgns "g" (constDyn $ groupAttrs pos) $ do
        rEl <- showSquare cell
        dEl <- showCellDetail pos cell
        return $ leftmost $ concatMap (mouseEv pos) (rEl ++ dEl)

showAndReturnCell :: MonadWidget t m => Pos -> Cell -> m (Event t Msg, Cell)
showAndReturnCell pos cell = do
    ev <- showCell pos cell
    return (ev, cell)

boardAttrs :: Map Text Text
boardAttrs =
    fromList
        [ ("width",  pack $ show $ w * cellSize)
        , ("height", pack $ show $ h * cellSize)
        , ("style",  "border:solid")
        ]

centerStyle =
    fromList [("style", "width: 75%; margin: 0 auto;text-align:center;")]

reactToPick :: (Board, Msg) -> Map Pos (Maybe Cell)
reactToPick (b, msg) =
    let (resultList, _) = runState (updateBoard msg) b
    in fromList resultList

boardWidget :: (RandomGen g) => (MonadWidget t m) => g -> m ()
boardWidget g = do
    let (initial, _) = runRand mkBoard g
    rec elAttr "div" centerStyle $ dyn (fmap (showFace . gameOver) board)
        elAttr "div" centerStyle $ text "Implemented using Reflex"
        let pick = switch $ (leftmost . elems) <$> current eventMap
            pickWithCells = attachPromptlyDynWith (,) board pick
            updateEv = fmap reactToPick pickWithCells
        (_, eventAndCellMap) <-
            elAttr "div" centerStyle $
            elSvgns "svg" (constDyn boardAttrs) $
            listHoldWithKey initial updateEv showAndReturnCell
        let board = fmap (fmap snd) eventAndCellMap
            eventMap = fmap (fmap fst) eventAndCellMap
    return ()

main :: IO ()
main = do
    g <- getStdGen
    let (gh:gs) = unfoldr (Just . split) g -- list of generators
    mainWidget $
     do
        -- 'rec' only here to get reset below board
        rec bEv <- zipListWithEvent const (fmap boardWidget gs) rEv
            widgetHold (boardWidget gh) bEv
            let btnElement = fst <$> (el' "button" $ text "Reset")
            let btnEvent = domEvent Click <$> btnElement
            rEv <- elAttr "div" centerStyle btnEvent
        return ()
