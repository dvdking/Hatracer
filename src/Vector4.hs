module Vector4
(
	Vector4,
	vector4FromList,	
	vector4ToList,
	dot
) where

data Vector4 = Vector4 Float Float Float Float deriving (Show)

vector4FromList :: [Float] -> Vector4
vector4FromList ls = Vector4 x y z w
	where 
		x = ls !! 0
		y = ls !! 1
		z = ls !! 2
		w = ls !! 3

vector4ToList :: Vector4 -> [Float]
vector4ToList (Vector4 x1 y1 z1 w1) = [x1, y1, z1, w1]

dot :: Vector4 -> Vector4 -> Float
dot (Vector4 x1 y1 z1 w1) (Vector4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2