module Scene
(
    Scene (..),
    Light (..),
    SceneElement(..),
    getFigure,
    getPosition
) where

import Figures
import Vector3

data Light = Directional Vector3 | Point Vector3 Float deriving (Show)

data SceneElement = SceneElement { figure :: Figure,
                                   position :: Vector3,
                                   color :: Vector3
                                 } deriving (Show)

data Scene = Scene { sceneElements :: [SceneElement],
                     lights :: [Light]
                   } deriving (Show)
getFigure :: SceneElement -> Figure
getFigure (SceneElement f _ _) = f

getPosition :: SceneElement -> Vector3
getPosition (SceneElement _ p _) = p
