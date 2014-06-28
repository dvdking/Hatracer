module Vector4
(
	Vector4,
	fromList,	
	toList,
	dot
) where

data Vector4 = Vector4 Float Float Float Float deriving (Show)

fromList :: [Float] -> Vector4
fromList ls = Vector4 x y z w
	where 
		x = ls !! 0
		y = ls !! 1
		z = ls !! 2
		w = ls !! 3

toList :: Vector4 -> [Float]
toList (Vector4 x1 y1 z1 w1) = [x1, y1, z1, w1]

dot :: Vector4 -> Vector4 -> Float
dot (Vector4 x1 y1 z1 w1) (Vector4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2