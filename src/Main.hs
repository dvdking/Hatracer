import Data.List
import Vector3
import Matrix4

data Figure = Sphere Vector3 Float deriving (Show)
data Light = Directional Vector3 | Point Vector3 Float deriving (Show)


data Scene = Scene { figures :: [Figure],
                     lights :: [Light]
                   } deriving (Show)

main :: IO ()
main = do
	return()
	
