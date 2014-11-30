module Gravity where

import Prim

import Control.Applicative
import Control.Monad
import Data.List
import Debug.Trace
import Graphics.UI.GLUT
import System.Random
import Text.Printf

pLAYER_ID :: ID
pLAYER_ID = 1

-- mass of the sun is 1.989e30 kg
--             earth is 5.972e24 kg @149.59787 million km
--             jupiter 1.898e27 kg
initGSt :: GSt
initGSt = GSt 1 False 10 [] ipEmpty False (300,300) (max_id + 1) g [] objs
  where objs = plr_sh : zipWith 
                  (\ix o -> o {oId = ix}) [pLAYER_ID + 1 ..] (plas ++ rocks)
  
        plas = [ planet (-1,-3) 0 (0, 0.001) 0.9 12000 1.2 (1.0,1.0,0.5)
               , planet ( 3, 4) 0 (0,-0.001) 1.1  9000 0.9 (1.0,0.5,0.5) ]

        plr_sh :: Obj
        plr_sh = Obj pLAYER_ID (Vec 0 0) 0 (Vec 0 0) 0 shMass shRad 
              shColor
              (OTShip (Ship 100 100 0))
          where shRad = 0.2
                shMass = 1
                shColor = Color3 0.8 0.8 1.0

        max_id = maximum (map oId objs)

        planet (px,py) or (vx,vy) dr m r (cr,cg,cb) =
         Obj (-1) (Vec px py) or (Vec vx vy) dr m r (Color3 cr cg cb) OTRock

        (rocks,g) = foldr acc ([],mkStdGen 12007) [1 .. 10]
        acc i (as0,g0) = case randomRock g0 of
                           (a,gN) -> (a:as0,gN)

validateGSt :: GSt -> IO ()
validateGSt g = do
  let ids = map oId (gObjs g)
      dups = ids \\ nub ids
  when (not (null dups)) $
    fail "duplicate ids"


randomRock:: RandomGen g => g -> (Obj,g)
randomRock g0 = (a,gN)
  where ([cr,cg,cb],g1) = randomRN 3 (0.3,1.0) g0
        ([px,py],g2) = randomRN 2 (xMIN,xMAX) g1
        ([vx,vy],g3) = randomRN 2 (-0.001,0.001) g2
        (mx,g4) = randomR (0.01,0.02) g3
        rx = 2*mx
        gN = g4
        a = Obj (-1) (Vec px py) 0 (Vec vx vy) 0 mx rx (Color3 cr cg cb) OTRock

-- remove if unused
randomRNs :: (Random a, RandomGen g) => [g -> (a,g)] -> g -> ([a],g)
randomRNs = go
  where go [] g = ([],g)
        go (f:fs) g0 = result
          where (a,g1) = f g0
                result = case go fs g1 of
                          (as,gN) -> (a:as,gN)

randomRN :: (Random a, RandomGen g) => Int -> (a, a) -> g -> ([a],g)
-- randomRN n b g = randomRNs (replicate n (randomR b)) g
randomRN n b g = go n g
  where go 0 g = ([],g)
        go n g0 = result
          where (a,g1) = randomR b g0
                result = case go (n - 1) g1 of
                          (as,gN) -> (a:as,gN)

findObj :: ID -> GSt -> Maybe Obj
findObj id g = find (\o -> oId o == id) (gObjs g)

explodeObj :: GSt -> Obj -> GSt
explodeObj g o =
  g { gObjs = filter ((/=oId o) . oId) (gObjs g) ++ new_pieces
                   , gG = gN
                   , gNextId = next_id }
  where mo = oMass o
        vo = oVel o
        po = oPos o
        co = oColor o
        ro = oRadius o

        (new_pieces,gN,next_id) = mkPieces mo (gG g) (gNextId g) k
          where (k,g0) = randomR ((10::Int),20) (gG g)
        mkPieces 0 g i _ = ([],g,i)
        mkPieces m g i k
          | m < 0.05 || k == 0 = ([o],gN,i+1)
          where (o,gN) = mkPiece m g i
        mkPieces m g0 i k = (o:os,gN,i2) 
          where mc_bounds = (m/4,m/2)
                (cm,g1) = randomR mc_bounds g0
                (o,g2) = mkPiece cm g1 i
                (os,gN,i2) = mkPieces (m - cm) g2 (i+1) (k-1)

        mkPiece :: Val -> StdGen -> ID -> (Obj,StdGen)
        mkPiece mc g0 ic = (o,g2)
          where o = Obj ic pc 0 vc 0 mc rc co OTRock
                rc = clamp 0.05 20 (ro * mc / mo)
                ([dx,dy],g1) = randomRN 2 (-0.2,0.2) g0
                pc = po .+. (Vec dx dy)
                (sc,g2) = randomR (0.4,0.8) g1
                -- 1 - mc/mo: means that we dampen the bigger chunks'
                -- velocities more than smaller chunks
                vc = vclamp $ sc * (1 - mc/mo) .*. (pc .-. po)

-- advance an object k steps
predictPos :: Int -> ID -> GSt -> [Vec]
predictPos k id g = go k g
  where go 0 _ = []
        go k g =
          case find (\o -> oId o == id) (gObjs g) of
            Just o -> oPos o : go (k - 1) (updateSim (gTickRate g) g)
            Nothing -> []

updateSim :: Int -> GSt -> GSt
updateSim dtI g = gN { gTick = gTick g + dtI, gOrders = [] }
  where gN = mspl g []  $ updObjs $ gObjs g

        updObjs :: [Obj] -> [Obj]
        updObjs = map (adv dt . applyFs . applyOrds)

        applyFs :: Obj -> Obj
        applyFs o = o { oVel = vclamp tf, oOr = norm_or (oOr o + oDr o * dt) }
          where tf = oVel o .+. dt .*. totalF g o
                norm_or or
                  | or >= 360 = norm_or (or - 360)
                  | or < 0 = norm_or (or + 360)
                  | otherwise = or

        applyOrds :: Obj -> Obj
        applyOrds o = dampRot . chargeShip . procInp (gOrders g) $ o
          where inps = map snd (filter (\(id,inp) -> id == oId o) (gOrders g))
                procInp [] o = o
                procInp ((id,inp):inps) o
                  | id /= oId o = procInp inps o -- not us
                  | oShipFuel o <= 0 = procInp inps o -- no fuel 
                  | otherwise = burnFuel $
                      case inp of
                        ILeft -> o { oDr = min (10) (oDr o + 0.1) }
                        IRight -> o { oDr = max (-10) (oDr o - 0.1) }
                        IThrust -> o { oVel = vclamp (1.5 .*. oVel o) }
                        IBrake -> o { oVel = 0.5 .*. oVel o }
                        _ -> o
                  where burnFuel o = oShipModifyFuel o (subtract dt)
                chargeShip o = oShipModifyFuel o (+0.1*dt)
                dampRot o = o { oDr = oDr o * 0.9 }
        dt = fromIntegral dtI

-- advect an object (honoring bounds)
adv :: Val -> Obj -> Obj
adv dt o = o { oPos = Vec p2x p2y, oVel = Vec v2x v2y }
  where (Vec px py, Vec vx vy) = (oPos o, oVel o)
        (apx,apy) = (px + vx * dt, py + vy * dt)
        (p2x,v2x)
          | apx > xMAX = (xMIN, vx*0.1)
          | apx < xMIN = (xMAX, vx*0.1)
          | otherwise = (apx, vx)
        (p2y,v2y)
          | apy > yMAX = (yMIN, vy*0.1)
          | apy < yMIN = (yMAX, vy*0.1)
          | otherwise = (apy, vy)
        -- (p2x,v2x)
        --  | apx > xMAX = (xMAX, vx*0.1)
        --  | apx < xMIN = (xMIN, vx*0.1)
        --  | otherwise = (apx, vx)
        -- (p2y,v2y)
        --  | apy > yMAX = (yMAX, vy*0.1)
        --  | apy < yMIN = (yMIN, vy*0.1)
        --  | otherwise = (apy, vy)

-- merge-split objects
-- we traverse the list accumulating the reverse of the object list;
-- at the end we reverse the list back to the original
-- during traversal, for each object we try and 
mspl :: GSt -> [Obj] -> [Obj] -> GSt
mspl g pv [] = g { gObjs = reverse pv } 
mspl g pv (o:os) = case (replFst pv, replFst os) of
                    (Just pv',_) -> mspl g pv' os
                    (_,Just os') -> mspl g pv  os'
                    _ -> mspl g (o:pv) os
  where tryAbsorb x
          | oMass x > 1000 && oMass o <= oMass x && dist2 o x < 0.01
          = Just (x { oMass = oMass x + oMass o })
          | otherwise = Nothing

        replFst [] = Nothing
        replFst (x:xs) = 
          case tryAbsorb x of
            Just x' -> Just (x':xs)
            Nothing -> 
              case replFst xs of
                Just xs' -> Just (x:xs')
                Nothing -> Nothing
-- update velocity
totalF:: GSt -> Obj -> Vec
totalF g o = vsum fs
  where others = filter (\o2 -> oId o2 /= oId o) (gObjs g)
        fs = map (fG o) others

-- F_G = G m1 m2 / r^2
-- F = m1 * a
-- G m1 m2 / r^2 = m * dv / dt
-- dv = G m1 m2 / r^2 / m * dt
fG :: Obj -> Obj -> Vec
fG o1 o2 = f .*. dv
  where (m1,m2) = (oMass o1,oMass o2)
        dv = oPos o2 .-. oPos o1
        d2 = max (dist2 o1 o2) 0.1
        -- f = gConst * m1 * m2 / d2
        f = gConst * m2 / d2



