module Matrix4
(
	Matrix4,
	column,
	row
) where
import Vector4

data Matrix4 = Matrix4 [Vector4] deriving (Show)

column :: Matrix4 -> Int -> Vector4
column (Matrix4 m) index = m !! index


row :: Matrix4 -> Int -> Vector4
row (Matrix4 m) index = fromList $ map ( !! index) [v1, v2, v3, v4] 
	where 
		v1 = toList $ m !! 0
		v2 = toList $ m !! 1
		v3 = toList $ m !! 2
		v4 = toList $ m !! 3

--multiply :: Matrix4 -> Matrix4 -> Matrix4
--multiply (Matrix4 m1) (Matrix4 m2)

--multiply :: Vector3 -> Matrix4 -> Vector3
--multiply 