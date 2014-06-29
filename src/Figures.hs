module Figures
(
	Figure(..),
	getRadius
) where

data Figure = Sphere Float deriving (Show)

getRadius :: Figure -> Float
getRadius (Sphere r) = r

