module Vector3
(
	Vector3(..),
	dot,
	normalize,
	magnitude,
	minus,
	getX,
	getY,
	getZ
) where

data Vector3 = Vector3 Float Float Float deriving (Show)

magnitude :: Vector3 -> Float
magnitude (Vector3 x1 y1 z1) = sqrt (x1*x1 + y1*y1 + z1*z1 )

normalize :: Vector3 -> Vector3
normalize (Vector3 x1 y1 z1) =  Vector3 x y z
	where 
		v = Vector3 x1 y1 z1
		l = magnitude v
		x = x1 / l
		y = y1 / l
		z = z1 / l


minus :: Vector3 -> Vector3 -> Vector3
minus (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 x y z
	where 
		x = x1 - x2
		y = y1 - y2
		z = z1 - z2

dot :: Vector3 -> Vector3 -> Float
dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

getX :: Vector3 -> Float
getX (Vector3 x _ _) = x

getY :: Vector3 -> Float
getY (Vector3 _ y _) = y

getZ :: Vector3 -> Float
getZ (Vector3 _ _ z) = z