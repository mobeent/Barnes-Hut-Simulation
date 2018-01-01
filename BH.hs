-- CS300-SP17 Assignment 2: Barnes Hut Simulation
-- Deadline: 24 Feb 9pm
-- Submission: via LMS only
--
import System.Environment
import Data.List
import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import Debug.Trace
import Data.Function (on)
--
-- PART 1: You are given many input files in the inputs directory and given
-- code can read and parse the input files. You need to run "cabal update
-- && cabal install glut" on your system to work on this assignment. Then
-- run "./ghc BH.hs && ./BH 1 < inputs/planets.txt" to run 1 iteration of
-- the algorithm and print updated positions. Replace 1 with any number to
-- run more iteration. You may also run it without an argument and it will
-- display the simulation using OpenGL in a window.
--
-- In the first part, you are to write the updateBody function to correctly
-- update body after 1 unit of time has passed. You have to correctly
-- update the position of a body by calculating the effect of all other
-- bodies on it. The placeholder implementation just moves the body left
-- without doing any physics. Read http://www.cs.princeton.edu/courses/
-- archive/fall03/cs126/assignments/nbody.html for help with physics. Try
-- simplyfying equations on paper before implementing them. You can compare
-- answers with the given binary solution.
--
-- Make helper functions as needed
type Vec2 = (Double, Double)
data Body = Body Vec2 Vec2 Double (Color3 Double)
updateBody :: (Foldable f) => f Body -> Body -> Body
updateBody bodies (Body (posx1, posy1) (velx1, vely1) mass1 clr1) =
  let (fx, fy) = foldl (calcforce (Body (posx1, posy1) (velx1, vely1) mass1 clr1)) (0,0) bodies
      ax = fx/mass1
      ay = fy/mass1
      vx = velx1 + ax
      vy = vely1 + ay
      px = posx1 + vx
      py = posy1 + vy
  in Body (px,py) (vx,vy) mass1 clr1

calcforce :: Body -> (Double, Double) -> Body -> (Double, Double)
calcforce (Body (posx1, posy1) (velx1, vely1) mass1 clr1) (foldx, foldy) (Body (posx2,posy2) (velx2, vely2) mass2 clr2) =
    let deltax = (posx2-posx1)
        deltay = (posy2-posy1)
        g = 6.67e-11
        r = sqrt((deltax**2) + (deltay**2))
        force = (g*mass1*mass2)/(r**2)
        fx = (force*deltax)/r
        fy = (force*deltay)/r
    in if ((posx1 == posx2) && (posy1 == posy2) && (mass1 == mass2))
       then (foldx, foldy)
       else (fx + foldx, fy + foldy)

-- PART 2: We will make a Quadtree to represent our universe. See
-- http://www.cs.princeton.edu/courses/archive/fall03/cs126/assignments/
-- barnes-hut.html for help on this tree. The QT structure has the the
-- length of one side of quadrant) for internal nodes. The makeQT function
-- has arguments: center, length of quadrant side, a function to find
-- coordinates of an element of the tree (e.g. extract position from a Body
-- object), a function to summarize a list of nodes (e.g. to calculate a
-- Body with position center of gravity and total mass), and the list of
-- nodes to put in tree.
--
-- Note that inserting all nodes at once is much easier than inserting one
-- by one. Think of creating the root node (given all nodes), and then
-- divide the nodes into quadrants and let recursive calls create the
-- appropriate trees. In this part you do not have to give a correct
-- implementation of the summary function
--
-- Now make QT member of Foldable typeclass. See what function you are
-- required to implement. Once this is done, change the tick function below
-- to create the tree and then pass it to updateBody function instead of a
-- list of bodies. No change to updateBody function should be needed since
-- it works with any Foldable.
data QT a = Internal Double a (QT a,QT a,QT a,QT a) | Leaf a | Nil
instance Foldable QT where
  foldl _ x Nil = x
  foldl f x (Leaf y) = f x y
  foldl f x (Internal _ _ (sw, se, nw, ne)) =
    let swresult = foldl f x sw
        seresult = foldl f swresult se
        nwresult = foldl f seresult nw
    in foldl f nwresult ne

makeQT :: Vec2 -> Double -> (a->Vec2) -> ([a]->a) -> [a] -> (QT a)
-- makeQT center radius getPos summarize bodies = Nil
makeQT _ _ _ _ [] = Nil
makeQT _ _ _ _ [x] = Leaf x
makeQT (px,py) radius getPos summarize bodies =
  let (sw,se,nw,ne) = checkquadrant (px,py) bodies getPos ([],[],[],[])
      swqt = makeQT (px-(radius/2), py-(radius/2)) (radius/2) getPos summarize sw
      seqt = makeQT (px+(radius/2), py-(radius/2)) (radius/2) getPos summarize se
      nwqt = makeQT (px-(radius/2), py+(radius/2)) (radius/2) getPos summarize nw
      neqt = makeQT (px+(radius/2), py+(radius/2)) (radius/2) getPos summarize ne
  in Internal radius (summarize bodies) (swqt, seqt, nwqt, neqt)

checkquadrant :: Vec2 -> [a] -> (a->Vec2) ->([a], [a], [a], [a]) -> ([a], [a], [a], [a])
checkquadrant _ [] _ (sw,se,nw,ne) = (sw,se,nw,ne)
checkquadrant (cx, cy) [x] getPos (sw,se,nw,ne)
  | ((px <= cx) && (py <= cy)) = ((x:sw),se,nw,ne)
  | ((px > cx) && (py <= cy)) = (sw,(x:se),nw,ne)
  | ((px <= cx) && (py > cy)) = (sw,se,(x:nw),ne)
  | ((px > cx) && (py > cy)) = (sw,se,nw,(x:ne))
  | otherwise = ((x:sw),se,nw,ne)
  where (px, py) = getPos x
checkquadrant (cx, cy) (x:xs) getPos (sw,se,nw,ne)
  | ((px <= cx) && (py <= cy)) = checkquadrant (cx,cy) xs getPos ((x:sw),se,nw,ne)
  | ((px > cx) && (py <= cy)) = checkquadrant (cx,cy) xs getPos (sw,(x:se),nw,ne)
  | ((px <= cx) && (py > cy)) = checkquadrant (cx,cy) xs getPos (sw,se,(x:nw),ne)
  | ((px > cx) && (py > cy)) = checkquadrant (cx,cy) xs getPos (sw,se,nw,(x:ne))
  | otherwise = checkquadrant (cx,cy) xs getPos ((x:sw),se,nw,ne)
  where (px, py) = getPos x

getPos :: Body -> Vec2
getPos (Body p _ _ _) = p
getMass :: Body -> Double
getMass (Body _ _ m _) = m
getPosbym :: Body -> Vec2
getPosbym (Body (x,y) _ m _) = (x*m, y*m)

summarize :: [Body] -> Body
summarize bodies =
  let positions = map getPosbym bodies
      mass = map getMass bodies
      totalmass = sum mass
      (px, py) = foldl1 centergrav positions
  in Body (px/totalmass,py/totalmass) (0,0) totalmass (Color3 0 0 0)

centergrav :: Vec2 -> Vec2 -> Vec2
centergrav (x,y) (x1,y1) = (x+x1,y+y1)

-- This functions takes a set of bodies and returns an updated set of
-- bodies after 1 unit of time has passed (dt=1)
tick ::Double -> [Body] -> [Body]
tick radius bodies =
  let tree = makeQT (0,0) radius getPos summarize bodies
  in checkthreshfunc bodies tree

checkthreshfunc :: [Body] -> (QT Body) -> [Body]
checkthreshfunc [] _ = []
checkthreshfunc ((Body (x,y) v m c):xs) tree =
  let threshold = 0.5
      checkthresh radius centerbody = radius/(dist (Body (x,y) v m c) centerbody) < threshold
  in (updateBody (BH checkthresh tree) (Body (x,y) v m c)):(checkthreshfunc xs tree)

-- PART 3: Now we create another datatype that contains a quadtree and a
-- function which given radius and a summarized body (containing center of
-- gravity and total mass) returns true if the summarized body is a good
-- enough approximation. Use 0.5 as threshold.
--
-- Make a correct summarize function to pass to makeQT above and then make
-- BH an instance of Foldable typeclass as well. However this instance
-- should use the internal node if the predicate function returns true and
-- recurse only if it returns false. Make sure to recurse over a BH type
-- variable. If your implementation is correct, you will be as fast as the
-- provided binary BH2 on large inputs like galaxy1.txt
dist :: Body -> Body -> Double
dist (Body (px1, py1) _ _ _) (Body (px2, py2) _ _ _) = sqrt((px2-px1)**2 + (py2-py1)**2)

data BH a = BH (Double -> a -> Bool) (QT a)
instance Foldable BH where
  foldl _ x (BH _ Nil) = x
  foldl f x (BH _ (Leaf y)) = f x y
  foldl f x (BH checkthresh (Internal radius centerbody (sw, se, nw, ne)))
    | checkthresh radius centerbody = f x centerbody
    | otherwise =
    let swresult = foldl f x (BH checkthresh sw)
        seresult = foldl f swresult (BH checkthresh se)
        nwresult = foldl f seresult (BH checkthresh nw)
    in foldl f nwresult (BH checkthresh ne)

---------------------------------------------------------------------------
-- You don't need to study the code below to work on the assignment
---------------------------------------------------------------------------
main :: IO ()
main = do
    (_,args) <- getArgsAndInitialize
    stdin <- getContents
    uncurry (mainChoice args) (parseInput stdin)

mainChoice :: [String] -> Double -> [Body] -> IO ()
mainChoice (iter:_) r bodies = putStr $ applyNtimes r bodies (read iter)
mainChoice [] r bodies = do
    createWindow "Barnes Hut"
    windowSize $= Size 700 700
    bodiesRef <- newIORef bodies
    ortho2D (-r) r (-r) r
    displayCallback $= (display r bodiesRef)
    addTimerCallback 10 (timer r bodiesRef)
    mainLoop

applyNtimes :: Double -> [Body] -> Int -> String
applyNtimes r bodies n = (unlines.map show) (iterate (tick r) bodies !! n)

parseInput :: String -> (Double, [Body])
parseInput input =
    let (cnt:r:bodies) = lines input
    in (read r, map read (take (read cnt) bodies))

dispBody :: Body -> IO ()
dispBody (Body (x,y) _ _ rgb) = color rgb >> vertex (Vertex2 x y)

display :: Double -> IORef [Body] -> IO ()
display r bodiesRef = do
    clear [ColorBuffer]
    bodies <- get bodiesRef
    renderPrimitive Points (mapM_ dispBody bodies)
    flush

timer :: Double -> IORef [Body] -> IO ()
timer r bodiesRef = do
    postRedisplay Nothing
    bodies <- get bodiesRef
    bodiesRef $= tick r bodies
    addTimerCallback 10 (timer r bodiesRef)

instance Read Body where
    readsPrec _ input =
        let (x:y:vx:vy:m:r:g:b:rest) = words input
        in (\str -> [(Body (read x,read y) (read vx,read vy) (read m)
            (Color3 ((read r)/255) ((read g)/255) ((read b)/255)),
            unwords rest)]) input

instance Show Body where
    show (Body (x,y) (vx,vy) _ _) =
        "x=" ++ show x ++ " y=" ++ show y ++ " vx=" ++
            show vx ++ " vy=" ++ show vy
