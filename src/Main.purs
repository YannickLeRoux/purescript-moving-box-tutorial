module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)

import Web.DOM.Document (Document, createElement) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (appendChild) as DOM

import Web.HTML (window) as HTML
import Web.HTML.Window as HTML.Window
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.HTML.HTMLElement as HTML.HTMLElement

import Direction (Direction(..))

foreign import setStyleProp :: String -> String -> DOM.Element.Element -> Effect Boolean

type State = { dirHor :: Direction
    , dirVert :: Direction
    , position :: Tuple Number Number
    , rafId :: HTML.Window.RequestAnimationFrameId
}

createBoxElement :: String -> DOM.Document -> Effect DOM.Element.Element
createBoxElement id document = do
    boxEl <- DOM.createElement "div" document
    DOM.Element.setId id boxEl
    DOM.Element.setClassName "box" boxEl
    _ <- setStyleProp "position" "relative" boxEl
    _ <- setStyleProp "width" "5em" boxEl
    _ <- setStyleProp "height" "5em" boxEl
    _ <- setStyleProp "background" "#ff4242" boxEl
    pure boxEl

getNewDirectionAndDist :: Direction -> Number -> Int -> Effect (Tuple Direction Number)
getNewDirectionAndDist dir distValPx widthOrHeight = case dir of
                            RightDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then pure (Tuple LeftDir distValPx)
                                else pure $ Tuple RightDir (distValPx + 9.0)
                            LeftDir -> if distValPx <= 0.0
                                then pure (Tuple RightDir distValPx)
                                else pure $ Tuple LeftDir (distValPx - 9.0)
                            DownDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then pure (Tuple UpDir distValPx)
                                else pure $ Tuple DownDir (distValPx + 9.0)
                            UpDir -> if distValPx <= 0.0
                                then pure (Tuple DownDir distValPx)
                                else pure $ Tuple UpDir (distValPx - 9.0)

moveBox :: Direction -> Direction -> DOM.Element.Element -> Ref State -> Effect Unit
moveBox hDir vDir el stateRef = do
    -- Read state
    state <- read stateRef

    -- Move box
    let Tuple distValPxHor distValPxVert = state.position
        distStrHor = (show distValPxHor) <> "px"
        distStrVert = (show distValPxVert) <> "px"

    _ <- setStyleProp "transform" ( "translate(" <> distStrHor <> ", " <> distStrVert <> ")" ) el

    w <- HTML.window
    width <- HTML.Window.innerWidth w
    height <- HTML.Window.innerHeight w

    Tuple hDirection newHDist <- getNewDirectionAndDist hDir distValPxHor width
    Tuple vDirection newVDist <- getNewDirectionAndDist vDir distValPxVert height
    let newPosition = Tuple newHDist newVDist

    -- Call next frame
    animationFrameId <- HTML.Window.requestAnimationFrame (moveBox hDirection vDirection el stateRef) w

    -- Update state
    write { dirHor: hDirection
        , dirVert: vDirection
        , position: newPosition
        , rafId: animationFrameId
    } stateRef

execFrame :: Direction -> Direction -> DOM.Element.Element -> Ref State -> Effect Unit
execFrame hDir vDir el stateRef = moveBox hDir vDir el stateRef

main :: Effect Unit
main = do
  -- Get window and document objects
  w <- HTML.window
  d <- HTML.Window.document w
  mBody <- HTML.body d
  defaultElem <- (DOM.createElement "span" (HTML.toDocument d))

  -- Create frame that does nothing just to get default frame id
  defaultId <- (HTML.Window.requestAnimationFrame (pure unit) w)

  -- Default state
  stateRef <- new {
    dirHor: RightDir,
    dirVert: DownDir,
    position: Tuple 0.0 0.0,
    rafId: defaultId
  }

  let b = case mBody of
        Nothing -> DOM.Element.toNode (defaultElem)
        Just b'  -> HTML.HTMLElement.toNode b'

  boxEl <- createBoxElement "the-box" $ HTML.toDocument d
  newBody <- DOM.appendChild (DOM.Element.toNode boxEl) b

  frameId <- HTML.Window.requestAnimationFrame (
    execFrame RightDir DownDir boxEl stateRef
  ) w
  pure unit