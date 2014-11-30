module UI where

import Draw
import Gravity
import Prim

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Debug.Trace
import Graphics.UI.GLUT
import System.Environment
import System.Exit
import Text.Printf

fQ :: Timeout
fQ = 30


run :: [String] -> IO ()
run as0 = do
  (_,_) <- getArgsAndInitialize
  w <- createWindow "Gravity"
  windowSize $= Size 800 800
  windowPosition $= Position 20 20
  t <- get elapsedTime
  validateGSt initGSt
  gio <- newIORef initGSt
  let init sz@(Size w h) = do
        putStrLn $ "init " ++ show w ++ " x " ++ show h
        viewport $= (Position 0 0, sz)
        matrixMode $= Modelview 0
        loadIdentity
        matrixMode $= Projection
        loadIdentity
        ortho2D (frV xMIN) (frV xMAX) (frV yMIN) (frV yMAX)
        modifyIORef gio $ \g -> g { gSize = (fromIntegral w, fromIntegral h) }
        -- ortho2D (-10) 10 (-10) 10
        -- ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
  reshapeCallback $= Just init
  let modifyGStNR f = modifyIORef gio f

      modifyGSt f = modifyGStNR f >> redisplay gio

      debug = do
        g <- readIORef gio
        print $ g { gObjs = [] }
        forM_ (zip [1..] (gObjs g)) $ \(ix,o) -> do
          putStrLn $ show ix ++ ". " ++ fmtObj o

      toggleRunning = do
          g <- readIORef gio
          when (not (gRunning g)) $
            addTimerCallback fQ (tick gio)
          modifyGSt $ \g -> g { gRunning = not (gRunning g) }

      stepSim = do
        g <- readIORef gio
        when (gRunning g) $ -- stop running
          toggleRunning
        stepSimBy gio (gTickRate g)

      modifyTickRate fr = modifyGSt func
        where func g = g { gTickRate = new_tr }
                where new_tr = clamp 1 200 (fr (gTickRate g))

      selectPrev = modifyGSt f
        where f g
                | null ids = select []
                | otherwise = case gSelected g of
                                [] -> select $ [last ids]
                                (x:_) -> select $ findPred x ids
                where ids :: [ID]
                      ids = map oId (gObjs g)

                      select s = g { gSelected = s }

                      findPred x (i1:i2:is)
                       | i2 == x = [i1]
                       | otherwise = findPred x (i2:is)
                      findPred x _ = [head ids]

      selectNext = modifyGSt f
        where f g 
                | null ids = select []
                | otherwise = case gSelected g of
                                [] -> select [head ids]
                                (x:_) -> select (findSucc x ids)
                where ids :: [ID]
                      ids = map oId (gObjs g)

                      select s = g { gSelected = s }

                      findSucc x (i1:i2:is)
                        | i1 == x = [i2]
                        | otherwise = findSucc x (i2:is)
                      findSucc x _ = [head ids]
   
      explodeSelected = modifyGSt f
        where f g = case gSelected g of
                     [x] -> case findObj x g of
                              Nothing -> g
                              Just o -> (explodeObj g o) { gSelected = [] }
                     [] -> g

      screenToWorld :: Position -> IO Vec
      screenToWorld (Position px py) = do
        (_,Size w h)<- get viewport
        return $! Vec
          (xMIN + (xMAX - xMIN) * (fromIntegral px / fromIntegral w))
          (yMAX + (yMIN - yMAX) * (fromIntegral py / fromIntegral h))

  let kbMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
      kbMouse k st _ _
        | is_input_key = modifyGStNR updKeys
        where (inp,is_input_key) = 
                case k of
                  SpecialKey KeyLeft -> key ILeft
                  SpecialKey KeyRight-> key IRight
                  SpecialKey KeyUp -> key IThrust
                  SpecialKey KeyDown -> key IBrake
                  SpecialKey KeyShiftL -> key IShield
                  Char ' ' -> key IShoot
                  _ -> (error "kbMouse unreachable", False) 
                where key i = (i,True)

              updKeys g
                | st == Down = g { gInpsDown = ipAdd (gInpsDown g) inp }
                | otherwise = g { gOrders = (pLAYER_ID,inp) : gOrders g
                                , gInpsDown = ipRemove (gInpsDown g) inp }


      kbMouse (Char ch) Down m _ = do
        g <- readIORef gio
        case ch of
          'h' -> putStrLn $ 
                    "sync keys are: \n" ++
                    "  a - animate\n" ++
                    "  d - dump debug info\n" ++
                    "  [,.<>] - adjust simulation rate\n" ++
                    "  [\\[\\]] - selected next, prev obj\n" ++
                    "  [ESC] clear selected\n" ++
                    "  [qQ] exit\n" ++
                    "  ` single step sim\n"
          'a' -> toggleRunning
          'd' -> debug
          'x' -> explodeSelected
          ',' -> modifyTickRate (subtract 1)
          '<' -> modifyTickRate (subtract 10)
          '.' -> modifyTickRate (+1)
          '>' -> modifyTickRate (+10)
          '`' -> stepSim
          '[' -> selectPrev
          ']' -> selectNext
          '\ESC' -> modifyGSt $ \g -> g {gSelected = []}
          c | c `elem` "qQ" -> quit gio
          c -> putStrLn $ "no action for " ++ show c
      kbMouse (MouseButton LeftButton) Up m pos = do
        g <- readIORef gio
        wcs <- screenToWorld pos
        modifyGSt $ \g -> g { gSelected = take 1 $ selectNearestIds g wcs }
      -- kbMouse (SpecialKey k) Up _ _ = do
      --  let v = case k of
      --            KeyUp    -> Vec 0 1
      --            KeyDown  -> Vec 0 (-1)
      --            KeyLeft  -> Vec (-1) 0
      --            KeyRight -> Vec 1 0
      --            _ -> Vec 0 0
      --  let nudgeObj g o 
      --        | oId o `elem` gSelected g = o { oVel = s .*. v .+. oVel o }
      --        | otherwise = o
      --        where s = min (1000 / oMass o) 0.25
      --  modifyGSt $ \g -> g { gObjs = map (nudgeObj g) (gObjs g) }

      kbMouse k kst m _ = return ()

  keyboardMouseCallback $= Just kbMouse

  displayCallback $= do 
    -- clearColor $= Color4 1 1 1 1 
    -- clear [ ColorBuffer, DepthBuffer ]
    -- loadIdentity
    readIORef gio >>= drawGSt
    flush
  -- addTimerCallback fQ (tick gio)
  mainLoop

redisplay :: IORef GSt -> IO ()
redisplay gio = do
  g <- readIORef gio
  when (gExit g) $ exitSuccess  -- leaveMainLoop
  when (not (gRunning g)) $ do
    postRedisplay Nothing

stepSimBy :: IORef GSt -> Int -> IO ()
stepSimBy gio dt = do
--  putStrLn $ "stepping " ++ show dt
--  , gInpsDown :: !InputSet
  let downKeyOrds :: GSt -> [(ID,Input)]
      downKeyOrds g = map ((,) pLAYER_ID) (ipToList (gInpsDown g))
  modifyIORef gio $ 
    \g -> updateSim dt (g {
                  gOrders = reverse (gOrders g) ++ downKeyOrds g
                })
  redisplay gio

quit :: IORef GSt -> IO ()
quit gio = do
  modifyIORef gio $ \g -> g { gExit = True }
  redisplay gio

tick :: IORef GSt -> IO ()
tick gio = do
  g <- readIORef gio
  -- t <- get elapsedTime
  stepSimBy gio (gTickRate g)
  when (gRunning g) $ addTimerCallback fQ (tick gio)
  postRedisplay Nothing


