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

dep = -30

moving :: Double -> Double -> SF () (Double,Double)
moving p0 v0 = proc () -> do
  v' <- integral -< 0
  v  <- arr (+ v0) -< v'
  p' <- integral -< v
  p  <- arr (+ p0) -< p'
  returnA -< (p,v)

ha1 :: Double -> Double -> Double -> Double -> SF () (Double,Double,Double,Double)
ha1 px0 py0 vx0 vy0 = switch (traj px0 py0 vx0 vy0) ssfunc
  where traj px' py' vx' vy' = proc input -> do
               (px,vx) <- moving px' vx' -< input
               (py,vy) <- moving py' vy' -< input
               event <- edge -< (px <= -10 || px >= 15 || py <= -10 || py >= 15)
               returnA -< ((px,py,vx,vy),event `tag` (px,py,vx,vy))
        ssfunc :: (Double,Double,Double,Double) -> SF () (Double,Double,Double,Double)
        ssfunc (px',py',vx',vy') = if(px' <= -10 || px' >= 15)
                                     then if(py' <= -10 || py' >= 15)
                                            then ha1 px' py' (-vx') (-vy')
                                            else ha1 px' py' (-vx') vy'
                                     else if(py' <= -10 || py' >= 15)
                                            then ha1 px' py' vx' (-vy')
                                            else ha1 px' py' vx' vy'

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

clearAndRender :: (Double,Double,Double,Double) -> IO ()
clearAndRender (px,py,vx,vy) = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    color white
    renderBall1 $ vector3 (unsafeCoerce px) (unsafeCoerce py) dep 

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

draw :: SF (Double,Double,Double,Double) (IO ())
draw = arr clearAndRender

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
    oldTime'  <- get oldTime
    let dt = (fromIntegral $ newTime' - oldTime')/1000
    react rh (dt, Nothing)
    writeIORef oldTime newTime'
    return ()

simulate :: SF (Event ()) (IO ())
simulate = discardInputs >>> (ha1 (-10) (-10) 40 10) >>> draw

discardInputs :: SF (Event ()) ()
discardInputs = arr $ const ()