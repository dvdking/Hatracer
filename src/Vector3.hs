module Vector3
(
	Vector3,
	dot
) where

data Vector3 = Vector3 Float Float Float deriving (Show)

dot :: Vector3 -> Vector3 -> Float
dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2