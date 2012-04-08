import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import GHC.Float
import System.Random
import Control.Parallel

--sources :: [(GLfloat, GLfloat, GLfloat)]
--sources = [(0.0, 0.0, 1.0), (0.4, 0.8, 0.8), ((-0.55), -0.5, 0.2),((0.55), -0.55, 0.2)]

main = do 
  (progname, _) <- getArgsAndInitialize
  createWindow "Metabollar is cool"
  windowSize $= Size 800 600 

  -- State-strore the created viewport so we can adjust the projection
  -- and the area being rendered correctly.
  viewportTopLeft <- newIORef ((-1.0, -1.0) :: (GLfloat, GLfloat))
  viewportSize <- newIORef ((2.0, 2.0) :: (GLfloat, GLfloat))

  reshapeCallback $= Just (reshape viewportTopLeft viewportSize)

  -- Set things up for interesting idle animation
  simTime <- newIORef (0.0 :: GLfloat)
  let numBoids = 5
  rBoids <- randomBoids numBoids
  boids <- newIORef rBoids

  idleCallback $= Just (idle simTime boids)
  displayCallback $= (display viewportTopLeft viewportSize boids)
  mainLoop

boidsToSources :: [Boid] -> [(GLfloat, GLfloat, GLfloat)]
boidsToSources a = map boidToSource a
  where boidToSource (Boid (Vector2 x y) _) = (x,y,0.5)

-- Type for the flocking algorithm
data Boid = Boid {
    boidPosition  :: Vector2(GLfloat)
  , boidDirection :: Vector2(GLfloat)
  } deriving Show

randomBoids :: Int -> IO ([Boid])
randomBoids 0 = do
  return []
randomBoids n = do
  b <- randomBoid 
  bs <- (randomBoids (n-1))
  return (b : bs)

randomBoid = do
  pos <- randomVector
  vel <- randomVector
  return (Boid pos vel)

randomVector = do
  x <- randomRIO(-1.0, 1.0)
  y <- randomRIO(-1.0, 1.0)
  return (Vector2 x y)

idle :: IORef (GLfloat) -> IORef([Boid]) -> IO ()
idle simTime boids = do
  let dt = 0.01 :: GLfloat
  t <- get simTime
  b <- get boids

  simTime $= dt + t
  boids $= step dt b
  postRedisplay Nothing

display viewportTopLeft viewportSize boids = do 
  clear [ColorBuffer]
  tl <- get viewportTopLeft
  size <- get viewportSize
  b <- get boids
  
  let fieldSources = boidsToSources b
  let triangles = (concat(map (makeTriangle fieldSources) (samplePoints tl size sampleDistance)))
  
  renderPrimitive Triangles $ mapM_ (\(x, y)->vertex$Vertex3 x y 0.0) triangles
  flush

reshape :: IORef (GLfloat, GLfloat) -> IORef (GLfloat, GLfloat) -> Size -> IO ()
reshape viewportTopLeft viewportSize s@(Size w h) = do
  viewport $= (Position 0 0, s)

  let scale = ((fromIntegral w) / 800.0)
  let aspect = ((fromIntegral h)/ (fromIntegral w)) 
  let scaledAspect = aspect * scale

  -- Set the viewport state so that we can use it to see how much of the field we need to calculate
  viewportTopLeft $= (-scale, -scaledAspect)
  viewportSize $= (scale * 2, scaledAspect * 2)

  -- Set the projection matrix
  projection (float2Double (-scale)) (float2Double scale) (float2Double (-(scaledAspect))) (float2Double scaledAspect) (-1.0) 1.0
  postRedisplay Nothing

-- Helper to set the ortographic projection matrix
projection xl xu yl yu zl zu = do
  matrixMode $= Projection
  loadIdentity
  ortho xl xu yl yu zl zu
  matrixMode $= Modelview 0

vadd :: Vector2(GLfloat) -> Vector2(GLfloat)-> Vector2(GLfloat)
vadd (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1+x2) (y1+y2)

vscale :: Vector2(GLfloat) -> GLfloat -> Vector2(GLfloat)
vscale (Vector2 x y) t = Vector2 (x*t) (y*t)

step :: GLfloat -> [Boid] -> [Boid]
step t a = map (stepBoid t) a
  where stepBoid t (Boid p v) = Boid (p `vadd` (v `vscale` t)) (v `vadd` (p `vscale` (-t)) ) 

--  0--1
--  | /|
--  |/ |
--  2--3
--
-- Cases found in:
-- http://www.cs.technion.ac.il/~cs234326/projects/MetaMan/metaballs.htm
--
makeTriangle :: [(GLfloat, GLfloat, GLfloat)] -> (GLfloat,GLfloat) -> [(GLfloat, GLfloat)]
makeTriangle sources (x1, y1) = case cornerStrengths of [p0@(_,_,True,_),  p1@(_,_,True,_),  p2@(_,_,True,_),  p3@(_,_,True,_) ] -> [(x1, y1), (x2, y1), (x1, y2),(x1, y2),(x2, y1),(x2, y2)]
                                                        [p0@(_,_,True,_),  p1@(_,_,False,_), p2@(_,_,False,_), p3@(_,_,False,_)] -> [(x1, y1), ix p0 p1, iy p0 p2]
                                                        [p0@(_,_,False,_), p1@(_,_,True,_),  p2@(_,_,False,_), p3@(_,_,False,_)] -> [ix p0 p1, (x2, y1), iy p1 p3]
                                                        [p0@(_,_,True,_),  p1@(_,_,True,_),  p2@(_,_,False,_), p3@(_,_,False,_)] -> [(x2, y1), (x1, y1), iy p0 p2, iy p0 p2, iy p1 p3, (x2,y1)]
                                                        [p0@(_,_,False,_), p1@(_,_,False,_), p2@(_,_,True,_),  p3@(_,_,False,_)] -> [(x1, y2), ix p2 p3, iy p0 p2]
                                                        [p0@(_,_,True,_),  p1@(_,_,False,_), p2@(_,_,True,_),  p3@(_,_,False,_)] -> [(x1, y1), ix p0 p1, (x1, y2), ix p0 p1, (x1,y2), ix p2 p3]
                                                        [p0@(_,_,False,_), p1@(_,_,True,_),  p2@(_,_,True,_),  p3@(_,_,False,_)] -> [ix p0 p1, (x2, y1), iy p1 p3, iy p0 p2, (x1, y2), ix p2 p3]
                                                        [p0@(_,_,True,_),  p1@(_,_,True,_),  p2@(_,_,True,_),  p3@(_,_,False,_)] -> [(x1, y1), (x2, y1), (x1, y2), (x2, y1), iy p1 p3, ix p2 p3, (x1,y2), (x2, y1), ix p2 p3]
                                                        [p0@(_,_,False,_), p1@(_,_,False,_), p2@(_,_,False,_), p3@(_,_,True,_) ] -> [(x2, y2), ix p2 p3, iy p1 p3]
                                                        [p0@(_,_,True,_),  p1@(_,_,False,_), p2@(_,_,False,_), p3@(_,_,True,_) ] -> [(x1, y1), ix p0 p1, iy p0 p2, (x2, y2), ix p2 p3, iy p1 p3]
                                                        [p0@(_,_,False,_), p1@(_,_,True,_),  p2@(_,_,False,_), p3@(_,_,True,_) ] -> [ix p0 p1, (x2, y1), (x2, y2), ix p0 p1, (x2, y2), ix p2 p3]
                                                        [p0@(_,_,True,_),  p1@(_,_,True,_),  p2@(_,_,False,_), p3@(_,_,True,_) ] -> [(x1, y1), (x2, y1), iy p0 p2, (x2, y1), ix p2 p3, iy p0 p2, (x2, y1), (x2, y2), ix p2 p3]
                                                        [p0@(_,_,False,_), p1@(_,_,False,_), p2@(_,_,True,_),  p3@(_,_,True,_) ] -> [iy p0 p2, iy p1 p3, (x1, y2), (x1, y2), (x2, y2), iy p1 p3]
                                                        [p0@(_,_,True,_),  p1@(_,_,False,_), p2@(_,_,True,_),  p3@(_,_,True,_) ] -> [(x1, y1), ix p0 p1, (x1, y2), (x1, y2), ix p0 p1, iy p1 p3, (x1, y2), iy p1 p3, (x2, y2)]
                                                        [p0@(_,_,False,_), p1@(_,_,True,_),  p2@(_,_,True,_),  p3@(_,_,True,_) ] -> [(x2, y2), ix p0 p1, iy p0 p2, ix p0 p1, (x2, y1), (x2, y2), iy p0 p2, (x1, y2), (x2, y2)]
                                                        [(_,_,_,_), (_,_,_,_), (_,_,_,_),     (_,_,_,_)] -> []
    where x2 = (x1 + sampleDistance)
          y2 = (y1 + sampleDistance) 
          cornerStrengths = map (fieldStrengthAndTreshold sources) [(x1,y1),(x2,y1),(x1,y2),(x2,y2)]
          ix f@(x, y, _, f0) t@(_, _, _, f1) = (interpolate x sampleDistance (interpolationFactor f0 f1 treshold), y)
          iy f@(x, y, _, f0) t@(_, _, _, f1) = (x, interpolate y sampleDistance (interpolationFactor f0 f1 treshold))

interpolate :: GLfloat -> GLfloat -> GLfloat -> GLfloat
interpolate x dx t = x + (dx * t)

-- Gets the delta with start, end, and current value
-- returns 0.0 -> 1.0 provided the value is between the endpoints
interpolationFactor :: GLfloat -> GLfloat -> GLfloat -> GLfloat
interpolationFactor a b c = (c - a) / (b - a)

fieldStrengthAndTreshold :: [(GLfloat, GLfloat, GLfloat)] -> (GLfloat, GLfloat)  -> (GLfloat, GLfloat, Bool, GLfloat)
fieldStrengthAndTreshold sources p@(x,y) = let strength = fieldStrength p sources in (x, y, strength > treshold, strength) 

samplePoints :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat -> [(GLfloat, GLfloat)]
samplePoints (tlx,tly) (szx,szy) d = [(x, y) | x <- [tlx,tlx+d..tlx+szx], y <- [tly,tly+d..tly+szy]]

sampleDistance :: GLfloat
sampleDistance = 0.02

screenTL :: (GLfloat, GLfloat)
screenTL = (-1.0, -1.0)

treshold :: GLfloat
treshold = 8.0

fieldStrength :: (GLfloat, GLfloat) -> [(GLfloat, GLfloat, GLfloat)]-> GLfloat
fieldStrength (x,y) sources = sum $ map (\(a,b,s) -> (1.0 / distanceSquared (x-a,y-b)) * s) sources

distanceSquared :: (GLfloat, GLfloat) -> GLfloat
distanceSquared (x,y) = x * x + y * y

