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

ha4 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> SF () (Double,Double,Double,Double,Double,Double,Double,Double)
ha4 px01 py01 vx01 vy01 px02 py02 vx02 vy02 = switch (traj px01 py01 vx01 vy01 px02 py02 vx02 vy02) ssfunc
  where  traj px1' py1' vx1' vy1' px2' py2' vx2' vy2' = proc input -> do
               (px1,vx1) <- moving px1' vx1' -< input
               (py1,vy1) <- moving py1' vy1' -< input
               (px2,vx2) <- moving px2' vx2' -< input
               (py2,vy2) <- moving py2' vy2' -< input
               ev11 <- edge -< px1 <= -10
               ev21 <- edge -< px1 >= 15
               ev31 <- edge -< py1 <= -10
               ev41 <- edge -< py1 >= 15
               ev12 <- edge -< px2 <= -10
               ev22 <- edge -< px2 >= 15
               ev32 <- edge -< py2 <= -10
               ev42 <- edge -< py2 >= 15
               hit  <- edge -< px1 - px2 <= 1 && px1 - px2 >= -1 && py1 - py2 <= 1 && py1 - py2 >= -1
               returnA -< ((px1,py1,vx1,vy1,px2,py2,vx2,vy2),if(hit /= NoEvent) 
                                                               then Event() `tag` (px1,py1,vx2,vy2,px2,py2,vx1,vy1)
                                                               else if(ev11 /= NoEvent || ev21 /= NoEvent)
                                                                      then if(ev31 /= NoEvent || ev41 /= NoEvent)
                                                                             then if(ev12 /= NoEvent || ev22 /= NoEvent) 
                                                                                    then if(ev32 /= NoEvent || ev42 /= NoEvent)
                                                                                           then Event() `tag` (px1,py1,-vx1,-vy1,px2,py2,-vx2,-vy2)
                                                                                           else Event() `tag` (px1,py1,-vx1,-vy1,px2,py2,-vx2,vy2)
                                                                                    else if(ev32 /= NoEvent || ev42 /= NoEvent)
                                                                                           then Event() `tag` (px1,py1,-vx1,-vy1,px2,py2,vx2,-vy2)
                                                                                           else Event() `tag` (px1,py1,-vx1,-vy1,px2,py2,vx2,vy2)
                                                                             else if(ev12 /= NoEvent || ev22 /= NoEvent)
                                                                                    then if(ev32 /= NoEvent || ev42 /= NoEvent)
                                                                                           then Event() `tag` (px1,py1,-vx1,vy1,px2,py2,-vx2,-vy2)
                                                                                           else Event() `tag` (px1,py1,-vx1,vy1,px2,py2,-vx2,vy2)
                                                                                    else if(ev32 /= NoEvent || ev42 /= NoEvent)
                                                                                           then Event() `tag` (px1,py1,-vx1,vy1,px2,py2,vx2,-vy2)
                                                                                           else Event() `tag` (px1,py1,-vx1,vy1,px2,py2,vx2,vy2)
                                                                      else if(ev31 /= NoEvent || ev41 /= NoEvent)
                                                                             then if(ev12 /= NoEvent || ev22 /= NoEvent) 
                                                                                    then if(ev32 /= NoEvent || ev42 /= NoEvent)
                                                                                           then Event() `tag` (px1,py1,vx1,-vy1,px2,py2,-vx2,-vy2)
                                                                                           else Event() `tag` (px1,py1,vx1,-vy1,px2,py2,-vx2,vy2)
                                                                                    else if(ev32 /= NoEvent || ev42 /= NoEvent)
                                                                                           then Event() `tag` (px1,py1,vx1,-vy1,px2,py2,vx2,-vy2)
                                                                                           else Event() `tag` (px1,py1,vx1,-vy1,px2,py2,vx2,vy2)
                                                                             else if(ev12 /= NoEvent || ev22 /= NoEvent) 
                                                                                    then if(ev32 /= NoEvent || ev42 /= NoEvent)
                                                                                           then Event() `tag` (px1,py1,vx1,vy1,px2,py2,-vx2,-vy2)
                                                                                           else Event() `tag` (px1,py1,vx1,vy1,px2,py2,-vx2,vy2)
                                                                                    else if(ev32 /= NoEvent || ev42 /= NoEvent)
                                                                                           then Event() `tag` (px1,py1,vx1,vy1,px2,py2,vx2,-vy2)
                                                                                           else NoEvent `tag` (px1,py1,vx1,vy1,px2,py2,vx2,vy2))   
         ssfunc (px1',py1',vx1',vy1',px2',py2',vx2',vy2') = ha4 px1' py1' vx1' vy1' px2' py2' vx2' vy2'

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

clearAndRender :: (Double,Double,Double,Double,Double,Double,Double,Double) -> IO ()
clearAndRender (px1,py1,vx1,vy1,px2,py2,vx2,vy2) = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    color white
    renderBall1 $ vector3 (unsafeCoerce px1) (unsafeCoerce py1) dep 
    renderBall2 $ vector3 (unsafeCoerce px2) (unsafeCoerce py2) dep
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
          renderBall2 = (color blue   >>) . (renderShapeAt $ Sphere' 0.5 20 20)
          points :: [(R, R, R)]
          points = [(-50, -2.5, dep+6), (50, -2.5, dep+6)]
          vtex3 (a, b, c) = Vertex3 a b c
          renderLine = renderPrimitive LineLoop $ mapM_ (vertex . vtex3) points

draw :: SF (Double,Double,Double,Double,Double,Double,Double,Double) (IO ())
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
simulate = discardInputs >>> (ha4 (-10) (-10) 40 10 10 10 (-15) (-25)) >>> draw

discardInputs :: SF (Event ()) ()
discardInputs = arr $ const ()
