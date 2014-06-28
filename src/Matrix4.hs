module Matrix4
(
	Matrix4,
	column,
	row
) where
import Vector4

data Matrix4 = Matrix4 [Vector4] deriving (Show)

matrixToList :: Matrix4 -> [[Float]]
matrixToList (Matrix4 m) = map (vector4ToList) m

matrixFromList :: [[Float]] -> Matrix4
matrixFromList (m) = Matrix4(map (vector4FromList) m)

column :: Matrix4 -> Int -> Vector4
column (Matrix4 m) index = m !! index


row :: Matrix4 -> Int -> Vector4
row (Matrix4 m) index = vector4FromList $ map ( !! index) [v1, v2, v3, v4] 
	where 
		v1 = vector4ToList $ m !! 0
		v2 = vector4ToList $ m !! 1
		v3 = vector4ToList $ m !! 2
		v4 = vector4ToList $ m !! 3



multiply :: Matrix4 -> Matrix4 -> Matrix4
multiply (Matrix4 m1) (Matrix4 m2) = Matrix4 (
    [ 
        Vector4((row m1 0) `dot` (column m2 0)
                (row m1 0) `dot` (column m2 1)
                (row m1 0) `dot` (column m2 2)
                (row m1 0) `dot` (column m2 3)),

        Vector4((row m1 1) `dot` (column m2 0)
                (row m1 1) `dot` (column m2 1)
                (row m1 1) `dot` (column m2 2)
                (row m1 1) `dot` (column m2 3)),

        Vector4((row m1 2) `dot` (column m2 0)
                (row m1 2) `dot` (column m2 1)
                (row m1 2) `dot` (column m2 2)
                (row m1 2) `dot` (column m2 3)),

        Vector4((row m1 3) `dot` (column m2 0)
                (row m1 3) `dot` (column m2 1)
                (row m1 3) `dot` (column m2 2)
                (row m1 3) `dot` (column m2 3)),
    ])

--multiply :: Vector3 -> Matrix4 -> Vector3
--multiply 