module Matrix4
(
	Matrix4,
	column,
	row,
    matrixToList,
    matrixFromList
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
        vector4FromList $ map (dot r0)[c0, c1, c2, c3],
        vector4FromList $ map (dot r1)[c0, c1, c2, c3],
        vector4FromList $ map (dot r2)[c0, c1, c2, c3],
        vector4FromList $ map (dot r3)[c0, c1, c2, c3]    
    ])

    where
        m1' = Matrix4 m1
        m2' = Matrix4 m2 
        r0 = (row m1' 0)
        r1 = (row m1' 1)
        r2 = (row m1' 2)
        r3 = (row m1' 3)
        c0 = (column m2' 0)
        c1 = (column m2' 1)
        c2 = (column m2' 2)
        c3 = (column m2' 3)



--multiply :: Vector3 -> Matrix4 -> Vector3
--multiply 