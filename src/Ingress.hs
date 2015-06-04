module Ingress where

import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import System.Random.MWC
import Data.Maybe
import Text.Printf


type Point = (Double, Double)

data Portal = Portal {
  name :: String,
  point :: Point
} deriving Show

type Board = Gr Portal ()

invalidLink :: (Portal, Portal) -> (Portal, Portal) -> Bool
invalidLink (p0, p1) (p2, p3) =
  let x_0 = fst $ point p0
      x_1 = fst $ point p1
      x_2 = fst $ point p2
      x_3 = fst $ point p3
      y_0 = snd $ point p0
      y_1 = snd $ point p1
      y_2 = snd $ point p2
      y_3 = snd $ point p3
      d_10x = x_1 - x_0
      d_10y = y_1 - y_0
      d_32x = x_3 - x_2
      d_32y = y_3 - y_2
      denom = d_10x * d_32y - d_32x * d_10y
      denomPos = denom > 0
      d_02x = x_0 - x_2;
      d_02y = y_0 - y_2;
      s_numer = d_10x * d_02y - d_10y * d_02x
      t_numer = d_32x * d_02y - d_32y * d_02x in
  not (denom == 0 || ((s_numer < 0) == denomPos) || ((t_numer < 0) == denomPos) ||
    ((s_numer > denom) == denomPos) || ((t_numer > denom) == denomPos))


linkDefenseBonus :: Int -> Double
linkDefenseBonus l = 4.0/9.0 * atan (min 5 $ fromIntegral l / exp 1)


maxLinks :: Board -> Double
maxLinks g =
  -313 * fromIntegral (length $ edges g)

linkDefense :: Board -> Double
linkDefense g = 
  -(sum $ map  (linkDefenseBonus . deg g) (nodes g))

showDefense :: Board -> String
showDefense g =
  let def = map (\n -> (n, linkDefenseBonus (deg g n)))
               (nodes g) :: [(Int, Double)] in
  concatMap (\(n,d) -> printf "%s: %.02f\n" (show $ name $ fromJust $ lab g n) d) def


addRndLink :: GenIO -> Board -> IO Board
addRndLink rng g = do
  let (mn, mx) = nodeRange g
  let es = edges g
  from <- uniformR (mn, mx) rng
  to   <- uniformR (mn, mx) rng
  if to == from || elem (from,to) es || elem (to,from) es
     then return g
     else do
       let p0 = fromJust $ lab g from 
       let p1 = fromJust $ lab g to
       if any (\(p2, p3) -> invalidLink (p0,p1) (fromJust $ lab g p2, fromJust $ lab g p3)) es
          then return g
          else return $ insEdge (from, to, ()) g 

delRndLink :: GenIO -> Board -> IO Board
delRndLink rng g = do
  let es = edges g
  r <- uniformR (0, length es - 1) rng
  let e = head $ drop r es
  return $ delEdge e g
  
nudge :: GenIO -> Board -> IO Board
nudge rng g = do
  r <- uniformR (0, 1::Double) rng

  g' <- if r >= 0.9 && edges g /= [] then delRndLink rng g
                                     else return g
  p <- uniformR (0, 1::Double) rng
  if p >= 0.1 || null (edges g) then addRndLink rng g'
                                  else return g'



setupBoard :: [Portal] -> Board
setupBoard ps =
  let ids = [1..]
      ns = zip ids ps
      g = empty :: Board in
  foldl add g ns
  where
    add g n = insNode n g

simAnnealing :: GenIO -> (Board -> Double) -> Board -> IO Board
simAnnealing rng cost g = do
  --let _T = 313 * 5 * (fromIntegral $ length $ nodes g)
  let _T = linkDefenseBonus 4 * fromIntegral (length $ nodes g)
  step2 _T g 100
  where
    step2 :: Double -> Board -> Int -> IO Board
    step2 _T alpha d = do
      beta <- nudge rng alpha
      alpha' <- if cost beta > cost alpha 
                   then do -- worse but we may stick with it
                     r <- uniformR (0,1) rng :: IO Double
                     if r < exp 1 ** (-((cost beta - cost alpha)/_T))
                        then return beta
                        else return alpha
                   else return beta

      let _T' = if d == 0 then coolFn _T
                          else _T
      let d' = if d == 0 then 100
                         else d - 1
      --printf "%f vs %f Temp %f\n" (cost beta) (cost alpha) _T
      if _T' < 0.000001 then return alpha'
                   else step2 _T' alpha' d'

    coolFn :: Double -> Double
    coolFn _T = _T * 0.95

portals :: [Portal]
portals = [
    Portal "Varbergs Stenen"   (57.105633,12.250836),
    Portal "Stadshotelet"      (57.105302,12.249892),
    Portal "Varberg 2010"      (57.105025,12.250848),
    Portal "Rådhuset"             (57.104968,12.251364),
    Portal "Varbergs Sparbank"    (57.104856,12.250602),
    Portal "Maj"                  (57.104786,12.251046),
    Portal "Gaddleken"            (57.104845,12.250013),
    Portal "Stadshotellet Mural"  (57.105034,12.249334),
    Portal "Asia Spa"             (57.105321,12.249423),
    Portal "Sailor"               (57.104747,12.254195),
    Portal "Trikorören"           (57.104606,12.250404),
    Portal "Cherub"               (57.104251,12.249035),
    Portal "The Sign"             (57.104283,12.248023),
    Portal "Table of Rocks"       (57.105688,12.24891),
    Portal "Divided"              (57.105668,12.249305),
    Portal "Lion Head"            (57.105775,12.25058),
    Portal "Kyrkan"               (57.105939,12.250242),
    Portal "Livscykeln"           (57.106128,12.249551),
    Portal "Torggatan"            (57.10572,12.252134),
    Portal "Kyrkogården"          (57.105906,12.254744),
    Portal "Ge Sverige 1 Kram"    (57.106689,12.254363),
    Portal "Hotell Gästis"        (57.107022,12.249135),
    Portal "Tidal Wave"           (57.105897,12.24915),
    Portal "Marknad Varberg"      (57.105992,12.248705),
    Portal "Fountain with Rocks"  (57.106233,12.248259),
    Portal "Fountain of Youth"    (57.106517,12.248063),
    Portal "Stilla Rörekse"       (57.106728,12.248265),
    Portal "Still Motion in Tree"  (57.106745,12.247914),
    Portal "hästar"               (57.106677,12.247371)
  ]


testDefenseLink :: IO ()
testDefenseLink = do
  let g = setupBoard portals 
  rng <- withSystemRandom (return :: GenIO -> IO GenIO)

  g' <- doIt rng g g 10
  printf "kaka %s\n" (show g')
  printf "%s" (showDefense g')
  dotOut g'
  where
    doIt :: GenIO -> Board -> Board -> Int -> IO Board
    doIt _ _ best 0 = return best
    doIt rng g best c = do
      g' <- simAnnealing rng linkDefense  g
      let best' = if linkDefense best > linkDefense g' then g'
                                                       else best
      printf "%s\n" (show g')
      doIt rng g best' (c - 1)

      

dotOut :: Board -> IO ()
dotOut g = do
  let dot = showDot (fglToDot g)
  printf "writing file\n"
  writeFile "file.dot" dot


