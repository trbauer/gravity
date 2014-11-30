module Draw(
    drawGSt
  ) where

import Gravity
import Prim

import Control.Applicative
import Control.Monad
import Data.List
import Graphics.UI.GLUT
import Text.Printf

drawGSt :: GSt -> IO ()
drawGSt g = do
  clear [ ColorBuffer, DepthBuffer ]
  drawUI g
  forM_ (gObjs g) (preservingMatrix . drawObj g)
  flush

drawAxes :: IO ()
drawAxes = do
  renderPrimitive Lines $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f 1 0 0
    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0 1 0
  lineStipple $= Just (4,0xAAAA)
  renderPrimitive Lines $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f (-1) 0 0
    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0 (-1) 0
  lineStipple $= Nothing
  color3f 1 1 1
 
drawUI :: GSt -> IO ()
drawUI g = do
 drawStringWindow (gSize g) 10 10 $ fmtGInfo g

drawShip :: GSt -> Obj -> Ship -> IO ()
drawShip g o s = do
  drawAxes
  color3f 1 1 1
  let (Vec px py) = oPos o
      (Vec vx vy) = oVel o
      selected = oId o `elem` gSelected g

  color (oColor o)
  renderPrimitive Polygon $ do
    vertex3f   0.20   (-0.3) 0
    vertex3f   0.15    (0.3) 0
    vertex3f (-0.15)   (0.3) 0
    vertex3f (-0.20)  (-0.3) 0


drawRock :: GSt -> Obj -> IO ()
drawRock g o = do
  color $ oColor o
  uscale $ oRadius o
  renderPrimitive Polygon $
    forM_ (circVerts o) $ \(x,y,z) -> vertex3f x y z
 
drawObj :: GSt -> Obj -> IO ()
drawObj g o = do
  -- putStrLn $ (show o)
  color3f 1 1 1
  let (Vec px py) = oPos o
      (Vec vx vy) = oVel o
      selected = oId o `elem` gSelected g

  -- draw the text for selected objects
  when selected $
    drawStringWorld (px + oRadius o + 0.1) (py + oRadius o/2) $ fmtObj o

  -- draw the object
  preservingMatrix $ do
    vecTrans (oPos o)
    rotate (vToF (oOr o)) (Vector3 0 0 1)
    preservingMatrix $
      case oType o of
        OTRock -> drawRock g o
        OTShip s -> drawShip g o s
     -- draw extra stuff on selected
    when selected $ do
      -- bounding box
      uscale $ oRadius o + 0.05
      color3f 0 1 0
      lineStipple $= Just (4,0xAAAA)
      renderPrimitive LineLoop $
        forM_ (circVerts o) $ \(x,y,z) -> do
          vertex3f x y z

  when selected $ do
    -- predicted path
    let ps = predictPos 100 (oId o) g
    renderPrimitive LineStrip $
      forM_ ps $ \(Vec px py) -> do
        vertex3f (vToF px) (vToF py) 0
    lineStipple $= Nothing

circVerts :: Obj -> [(GLfloat,GLfloat,GLfloat)]
circVerts o
  | oRadius o < 0.1 = circ3
  | oRadius o < 2.0 = circ12
  | otherwise = circ24

circ3 = circ 3
circ12 = circ 12
circ24 = circ 24
 
circ :: Int -> [(GLfloat,GLfloat,GLfloat)]
circ n = [(sin (2*pi*k/nd), cos (2*pi*k/nd), 0) | k <- [1..fromIntegral n]]
  where nd = fromIntegral n

drawStringWorld :: Val -> Val -> String -> IO ()
drawStringWorld x y s = do
  (_,Size _ h) <- get viewport
  let fh = 12 * (yMAX - yMIN) / fromIntegral h -- (yMAX - yMIN) / VIEW_H
  forM_ (zip (lines s) [y, y - fh ..]) $ \(ln,ln_y) -> do
    rasterPos $ Vertex2 (vToF x) (vToF ln_y)
    renderString Helvetica12 ln

drawStringWindow :: (Val,Val) -> Val -> Val -> String -> IO ()
drawStringWindow (w,h) x y s = do
  preservingMatrix $ do
    loadIdentity
    ortho2D 0 (frV w) (frV h) (0 :: GLdouble)
    let fh = 12
    forM_ (zip (lines s) [y, y + fh ..]) $ \(ln,ln_y) -> do
      rasterPos $ Vertex2 (vToF x) (vToF ln_y)
      renderString Helvetica12 ln

