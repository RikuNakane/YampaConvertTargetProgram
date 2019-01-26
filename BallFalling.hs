{-# LANGUAGE Arrows #-}

import FRP.Yampa
import Control.Concurrent
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities
import Unsafe.Coerce
import Data.IORef

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

type R = GLdouble

dep = -50

falling :: Double -> Double -> SF () (Double,Double)
falling y0 v0 = proc input -> do
  v' <-  integral     -< -10
  v  <-  arr (+ v0) -< v'
  y' <-  integral     -< v
  y  <-  arr (+ y0) -< y'
  returnA -< (y,v)

boundBall :: Double -> Double -> SF () (Double,Double)
boundBall y0 v0= switch (bb y0 v0) (\ (y,v) -> boundBall y (-v / 2))
  where bb y0' v0' = proc input -> do
          (y,v) <- falling y0' v0' -< input
          event <- edge -< y <= 0
          returnA -< ((y,v), event `tag` (y,v))

initGL :: IO ()
initGL = do
    getArgsAndInitialize
    initialWindowSize  $= Size 640 480
    initialWindowPosition $= Position 500 50
    createWindow "Shooting Game"
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered ]
    depthFunc          $= Just Less
    clearColor         $= Color4 0 0 0 0
    light (Light 0)    $= Enabled
    lighting           $= Enabled
    lightModelAmbient  $= Color4 0.5 0.5 0.5 1
    diffuse (Light 0)  $= Color4 1 1 1 1
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    reshapeCallback    $= Just resizeScene
    return ()

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  -- putStrLn "resizeScene"
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (w2/h2) 1 1000
  matrixMode $= Modelview 0
 where
   w2 =half width
   h2 =half height
   half z = realToFrac z / 2

clearAndRender :: (Double,Double) -> IO ()
clearAndRender (y,v) = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    color white
    renderLine
    renderBall1 $ vector3 (unsafeCoerce 0) (unsafeCoerce y) dep 

    flush
    where size2 :: R
          size2 = (fromInteger $ 6)/2
          green  = Color4 0.5 1 0.5 1 :: Color4 R
          greenG = Color4 0.5 1 0.5 1 :: Color4 R
          red    = Color4 1 0.5 0.5 1 :: Color4 R
          blue   = Color4 0.5 0.5 1 1 :: Color4 R
          white  = Color4 1 1 1 1 :: Color4 R
          renderShapeAt s p = preservingMatrix $ do
            translate $ G.Vector3 (0.5 - size2 + vector3X p)
                                  (0.5 - size2 + vector3Y p)
                                  (0.5 - size2 + vector3Z p)
            renderObject Solid s
          renderBall1 = (color red   >>) . (renderShapeAt $ Sphere' 0.5 20 20)
          points :: [(R, R, R)]
          points = [(-50, -2.5, dep+6), (50, -2.5, dep+6)]
          vtex3 (a, b, c) = Vertex3 a b c
          renderLine = renderPrimitive LineLoop $ mapM_ (vertex . vtex3) points

draw :: SF (Double,Double) (IO ())
draw = arr  (\ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel))

main :: IO ()
main = do
    oldTime <- newIORef (0 :: Int)
    rh <- reactInit (initGL >> return NoEvent) (\_ _ b -> b >> return False) simulate
    displayCallback $= return ()
    idleCallback $= Just (idle  oldTime rh)
    oldTime' <- get elapsedTime
    writeIORef oldTime oldTime' 
    mainLoop

idle :: IORef Int -> ReactHandle (Event ()) (IO ()) -> IO ()
idle oldTime rh = do
    newTime'  <- get elapsedTime
    threadDelay 100000
    oldTime'  <- get oldTime
    let dt = (fromIntegral $ newTime' - oldTime')/1000
    react rh (1, Nothing)
    writeIORef oldTime newTime'
    return ()

simulate :: SF (Event ()) (IO ())
simulate = discardInputs >>> (boundBall 100 0) >>> draw

discardInputs :: SF (Event ()) ()
discardInputs = arr $ const ()