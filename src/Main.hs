import Data.List
import Vector3
import Matrix4
import Ray	
import Scene
import Figures

render :: Scene -> IO ()
render scene = return()

main :: IO ()
main = do
	render $ Scene {
		sceneElements=[
				(SceneElement (Sphere 14) (Vector3 0 0 3) (Vector3 1 0 0))
			]
		, lights=[]
	}
	
