module Ray
(
    Ray(..),
    raycast
) where

import Scene
import Figures
import Vector3

data Ray = Ray Vector3 Vector3 deriving (Show)
data IteserectionInfo = IteserectionInfo { inteserectionPoint :: Vector3
                                         , sceneElement :: SceneElement	
                                         } deriving (Show)

raycast :: Ray -> [SceneElement] -> Maybe IteserectionInfo
raycast ray xs = findMin ray Nothing xs

findMin :: Ray -> Maybe IteserectionInfo -> [SceneElement] -> Maybe IteserectionInfo
findMin _ ii [] = ii
findMin ray m (x:xs) = findMin ray newElement xs
    where 
        isCloser' = isCloser ray m
        rayCastResult = (raycast' ray x);
        chooseResult' = chooseResult m rayCastResult
        newElement = chooseResult' $ isCloser' rayCastResult

isCloser :: Ray -> Maybe IteserectionInfo -> Maybe IteserectionInfo -> Maybe Bool
isCloser _ Nothing Nothing = Nothing
isCloser _ Nothing (Just e) = Just True
isCloser _ (Just e) Nothing = Just False
isCloser (Ray origin _) (Just (IteserectionInfo inteserectionPointMin _)) (Just (IteserectionInfo inteserectionPoint _)) = Just (newLength < oldLength)
    where
        newLength = magnitude (origin `minus` inteserectionPoint) 
        oldLength = magnitude (origin `minus` inteserectionPointMin)

chooseResult :: Maybe IteserectionInfo -> Maybe IteserectionInfo -> Maybe Bool -> Maybe IteserectionInfo
chooseResult Nothing Nothing _ = Nothing
chooseResult Nothing (Just i2) _ = Just i2
chooseResult (Just i1) Nothing _ = Just i1
chooseResult (Just i1) (Just i2) (Just True) = Just i1
chooseResult (Just i1) (Just i2) (Just False) = Just i2

raycast' :: Ray -> SceneElement -> Maybe IteserectionInfo
raycast' ray se = raycast'' ray (getFigure se) (getPosition se) se
    

raycast'' :: Ray -> Figure -> Vector3 -> SceneElement -> Maybe IteserectionInfo
raycast'' (Ray origin direction) (Sphere radius) pos se | discriminant < 0 = Nothing
                                                        | otherwise = let d = sqrt(discriminant) 
                                                                          t1 = (-b+discriminant)/2*a
                                                                          t2 = (-b-discriminant)/2*a 
                                                                      in if(t1 > 0) then  Just (createInteserectionInfo t1)
                                                                      else if(t2 > 0) then Just (createInteserectionInfo t2)
                                                                      else Nothing
    where
        ray = (Ray origin direction)
        f = normalize (pos `minus` origin) 

        a = direction `dot` direction
        b = 2*f `dot` direction
        c =  f `dot` f - radius*radius

        discriminant = b*b - 4*a*c

        createInteserectionInfo t = let x = getCollisionX ray t
                                        y = getCollisionY ray t
                                        z = getCollisionZ ray t
                                        in IteserectionInfo{inteserectionPoint=(Vector3 x y z), sceneElement=se }


getCollision :: (Vector3 -> Float) -> Ray -> Float -> Float
getCollision f (Ray origin direction) t = (f  origin) + (f direction)*t

getCollisionX :: Ray -> Float -> Float
getCollisionX = getCollision getX

getCollisionY :: Ray -> Float -> Float
getCollisionY = getCollision getY

getCollisionZ :: Ray -> Float -> Float
getCollisionZ = getCollision getZ