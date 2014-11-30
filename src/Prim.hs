module Prim where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Ord
import Graphics.UI.GLUT
import System.Random
import Text.Printf
import Data.Bits((.|.),(.&.))
import qualified Data.Bits as DB

type Val = Double
type ID = Int

data Vec = Vec {
    vX :: !Val
  , vY :: !Val
  } deriving (Show,Eq,Ord)

-- a generic physical object
-- the union type referenced by 'oType' contains
-- specific info
data Obj = Obj {
    oId :: !ID
  , oPos :: !Vec -- position
  , oOr :: !Val -- orientation (rotation)
  , oVel :: !Vec -- velocity
  , oDr :: !Val  -- change in rotation
  , oMass :: !Val -- mass
  , oRadius :: !Val -- bounding box for collisions
  , oColor :: !(Color3 GLfloat)
  , oType :: !ObjType -- object-specific info
  } deriving Show

data ObjType =
    OTRock 
  | OTShip !Ship
  deriving Show

data Ship = Ship {
    sHp :: !Val
  , sFuel :: !Val
  , sCargo :: !Val
  } deriving Show

data GSt = GSt {
    gTick :: !Int
  , gExit :: !Bool
--  , gLastTick :: !Int
  , gTickRate :: !Int -- in days
  , gOrders :: ![(ID,Input)]
  , gInpsDown :: !InputSet
  , gRunning :: !Bool
  , gSize :: (Val,Val)
  , gNextId :: !ID
  , gG :: StdGen
  , gSelected :: [ID]
  , gObjs :: ![Obj]
  } deriving Show

oShipModifyFuel :: Obj -> (Val -> Val) -> Obj
oShipModifyFuel o f = 
  case oType o of
    OTShip s -> o { oType = OTShip (s { sFuel = clamp 0 100 (f (sFuel s)) }) }
    _ -> o

oShipFuel :: Obj -> Val
oShipFuel o =
  case oType o of
    OTShip s -> sFuel s
    _ -> 0


class HasPos p where
  pos :: p -> Vec
instance HasPos Vec where
  pos = id
instance HasPos Obj where
  pos = oPos

oIntersect :: Obj -> Obj -> Bool
oIntersect o1 o2 = dist2 o1 o2 < dr*dr
  where dr = oRadius o1 + oRadius o2

gConst :: Val
gConst = 6.67384e-11
-- gConst = 0.00001 
-- dScale :: Val -- 1 world unit is dScale miles
-- dScale = 10

(xMIN,xMAX) = (-10,10) :: (Val,Val)
(yMIN,yMAX) = (-10,10) :: (Val,Val)
(vMIN,vMAX) = (-1,1) :: (Val,Val)



clamp :: Ord a => a -> a -> a -> a
clamp lo hi x
  | x < lo = lo
  | x > hi = hi
  | otherwise = x

-- clamp a velocity
vclamp :: Vec -> Vec
vclamp = vmap (clamp vMIN vMAX) (clamp vMIN vMAX)

-- clamp a position to world coordinates
-- wclamp :: Vec -> Vec
-- wclamp = vmap (clamp xMIN xMAX) (clamp yMIN yMAX)

fmtGInfo :: GSt -> String
fmtGInfo g = day_line ++ "\n" ++ tick_line ++ "\n" ++ sel_line
  where day_line  = printf "tick:       %10d" (gTick g)
        tick_line = printf "tick-rate:  %10d ticks/fr %s" (gTickRate g) tstr
          where tstr = if gRunning g then "" else "(stepping)"
        sel_line = "selected:   " ++
          case gSelected g of
            [] -> "(nothing)"
            ids -> show ids

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g (b :: GLfloat)

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO () 
vertex3f x y z = vertex $ Vertex3 x y z

color3fv :: (GLfloat,GLfloat,GLfloat) -> IO ()
color3fv (r,g,b) = color $ Color3 r g b

vertex3fv :: (GLfloat,GLfloat,GLfloat) -> IO () 
vertex3fv (x,y,z) = vertex $ Vertex3 x y z


vecTrans :: Vec -> IO ()
vecTrans (Vec x y) = translate $ Vector3 (vToF x) (vToF y) 0

uscale :: Val -> IO ()
uscale n = scale (vToF n) (vToF n) 1

-- frV :: Val -> GLfloat
frV :: Fractional b => Val -> b
frV = realToFrac

vToF :: Val -> GLfloat
vToF = frV

fmtObj :: Obj -> String
fmtObj o = printf "#%d (m=%.3f),\np:(%.4f,%.4f) @%.2f\nv: <%.5f,%.5f> @%.2f" 
              (oId o) (oMass o) px py (oOr o) vx vy (oDr o) ++ ship_info
  where (Vec px py) = oPos o
        (Vec vx vy) = oVel o
        ship_info =
          case oType o of
            OTShip s -> "\n" ++
              printf "hp: %.1f, fuel %.1f, carg: %.1f"
                  (sHp s) (sFuel s) (sCargo s) 
            _ -> ""

dist2 :: (HasPos a, HasPos b) => a -> b -> Val
dist2 o1 o2 = dx * dx + dy * dy
  where (Vec p1x p1y,Vec p2x p2y) = (pos o1, pos o2)
        (dx,dy) = (p2x - p1x, p2y - p1y)

vmap :: (Val -> Val) -> (Val -> Val) -> Vec -> Vec
vmap f g (Vec x y) = Vec (f x) (g y)

-- vec scale and add
infixl 6 .+.
(.+.) :: Vec -> Vec -> Vec
(.+.) (Vec x1 y1) (Vec x2 y2) = Vec (x1+x2) (y1+y2)
infixl 6 .-.
(.-.) :: Vec -> Vec -> Vec
(.-.) (Vec x1 y1) (Vec x2 y2) = Vec (x1-x2) (y1-y2)
infixl 7 .*.
(.*.) :: Val -> Vec -> Vec
(.*.) s (Vec x y) = Vec (s*x) (s*y)

vsum :: [Vec] -> Vec
vsum vs = Vec (csum vX) (csum vY)
  where csum f = sum $ map f vs

selectNearestIds :: GSt -> Vec -> [ID]
selectNearestIds g = map oId . selectNearestObjs g

selectNearestObjs :: GSt -> Vec -> [Obj]
selectNearestObjs g v = 
  map snd $
    sortBy (comparing fst) $
      filter (\(d2,o) -> d2 <= 0.1) $
        map (\o -> (dist2 v o, o)) (gObjs g)

data Input = 
    ILeft
  | IRight
  | IThrust
  | IBrake
  | IShield
  | IShoot
  deriving (Eq,Show,Enum)

newtype InputSet = IS Int
  deriving Eq
ipEmpty :: InputSet
ipEmpty = IS 0

ipAdd :: InputSet -> Input -> InputSet
ipAdd (IS bits) i = IS (bits .|. bit)
  where bit = 1 `DB.shiftL` fromEnum i
ipRemove :: InputSet -> Input -> InputSet
ipRemove (IS bits) i = IS (bits .&. DB.complement bit)
  where bit = 1 `DB.shiftL` fromEnum i

ipShow :: InputSet -> String
ipShow = ("ipFromList "++) . show . ipToList

ipToList :: InputSet -> [Input]
ipToList (IS bits) = filter (DB.testBit bits . fromEnum) [ILeft ..]

instance Show InputSet where
  show = ipShow

orToV :: Val -> Vec
orToV o = Vec (cos o) (sin o)




