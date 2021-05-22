{-# Language OverloadedStrings #-}

module Lumatone.Config where

import Control.Applicative
import Control.Monad
import Data.Ini
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.List

-- | Map from key numbers on a single board to sensible (Int, Int) coordinates, where the most southwest key is (0,0).
oneBoardCoordMap :: Map Int (Int, Int)
oneBoardCoordMap = Map.fromList (zip [0..55] (concat rows))
  where
    rows = -- Saner coordinates for the rows of a single 56-key Terpstra board.
      [[(k,8) | k <- [-4 .. -3]] -- OO
      ,[(k,7) | k <- [-3 .. 1]]  --  OOOOO
      ,[(k,6) | k <- [-3 .. 2]]  --  OOOOOO
      ,[(k,5) | k <- [-2 .. 3]]  --   OOOOOO
      ,[(k,4) | k <- [-2 .. 3]]  --   OOOOOO
      ,[(k,3) | k <- [-1 .. 4]]  --    OOOOOO
      ,[(k,2) | k <- [-1 .. 4]]  --    OOOOOO
      ,[(k,1) | k <- [0 .. 5]]   --     OOOOOO
      ,[(k,0) | k <- [0 .. 5]]   --     OOOOOO
      ,[(k,-1) | k <- [2 .. 6]]  --       OOOO
      ,[(k,-2) | k <- [5 .. 6]]  --         OO
      ]

-- | Map from board index and key number to sensible 2D coordinates. The most southwest key is (0,0).
lumatoneCoordMap :: Map Int (Map Int (Int, Int))
lumatoneCoordMap = Map.fromList [(b, Map.map (\(i,j) -> (7*b+i, (-2)*b+j)) oneBoardCoordMap) | b <- [0..4]]

-- | Unsafely look up a coordinates of a key. Handy when reconstructing layouts and you know the index will be valid by construction.
lookupCoords :: Int -> Int -> (Int, Int)
lookupCoords b k = maybe (error ("lookupCoords: bad indices: " ++ show (b,k))) id $ do
  m <- Map.lookup b lumatoneCoordMap
  Map.lookup k m

-- | Type for representing the contents of a .ltn configuration. Currently leaves out the non-key configuration.
data LumatoneConfig = LumatoneConfig
  { keyConfigs :: Map Int (Map Int KeyConf) -- ^ Board -> Index -> KeyConf
  }
  deriving (Eq, Show)

data KeyConf = KeyConf
  { keyConf_col :: Colour Double
  , keyConf_key :: Int
  , keyConf_chan :: Int
  }
  deriving (Eq, Show)

parseLumatoneConfig :: Ini -> Either String LumatoneConfig
parseLumatoneConfig ini = do
  bs <- forM [0..4] $ \b -> do
    ks <- forM [0..55] $ \i -> do
      let b' = "Board" <> T.pack (show b)
          i' = T.pack (show i)
      col <- parseColour =<< lookupValue b' ("Col_" <> i') ini
      key <- parseNumber =<< lookupValue b' ("Key_" <> i') ini
      chan <- parseNumber =<< lookupValue b' ("Chan_" <> i') ini
      return (i, KeyConf col key chan)
    return (b, Map.fromList ks)
  return $ LumatoneConfig { keyConfigs = Map.fromList bs }

parseNumber :: Text -> Either String Int
parseNumber v = case reads (T.unpack v) of
  [(n,"")] -> Right n
  _ -> Left ("Couldn't parse number: " ++ show v)

parseColour :: (RealFrac a, Floating a) => Text -> Either String (Colour a)
parseColour s =
  let s' = T.unpack s
  in case sRGB24reads s' of
       (c,""):_ -> Right c
       _ -> Left ("Couldn't parse colour: " ++ s')

keyConfAssocs :: (Int, KeyConf) -> Map Text Text
keyConfAssocs (n, k) =
  let n' = T.pack (show n)
  in Map.fromList
       [ ("Col_" <> n', T.pack (drop 1 (sRGB24show (keyConf_col k))))
       , ("Key_" <> n', T.pack (show (keyConf_key k)))
       , ("Chan_" <> n', T.pack (show (keyConf_chan k)))
       ]

boardAssocs :: Map Int KeyConf -> Map Text Text
boardAssocs m = Map.unions (map keyConfAssocs (Map.assocs m))

mergeAssocs :: Map Text Text -> [(Text, Text)] -> [(Text, Text)]
mergeAssocs m [] = Map.assocs m
mergeAssocs m ((k,v):xs) = case Map.lookup k m of
  Nothing -> (k,v) : mergeAssocs m xs
  Just v' -> (k,v') : mergeAssocs (Map.delete k m) xs

updateLumatoneConfig :: LumatoneConfig -> Ini -> Ini
updateLumatoneConfig cfg ini0 =
  foldl' (\ini (b,m) -> updateIni (T.pack ("Board" ++ show b)) (mergeAssocs (boardAssocs m)) ini) ini0 (Map.assocs (keyConfigs cfg))

updateIni :: Text -> ([(Text, Text)] -> [(Text, Text)]) -> Ini -> Ini
updateIni sect f ini = ini { iniSections = HM.alter (Just . f . maybe [] id) sect (iniSections ini) }

withColours :: (KeyConf -> Colour Double) -> LumatoneConfig -> LumatoneConfig
withColours f lc = withKeyConf f' lc
  where f' kc = kc { keyConf_col = f kc }

withKeyConf :: (KeyConf -> KeyConf) -> LumatoneConfig -> LumatoneConfig
withKeyConf f (LumatoneConfig m) = LumatoneConfig (Map.map (Map.map f) m)

withBoardKeyConf :: (Int -> KeyConf -> KeyConf) -> LumatoneConfig -> LumatoneConfig
withBoardKeyConf f (LumatoneConfig m) = LumatoneConfig (Map.mapWithKey (\b -> Map.map (f b)) m)

withBoardKeyConf' :: (Int -> Int -> KeyConf -> KeyConf) -> LumatoneConfig -> LumatoneConfig
withBoardKeyConf' f (LumatoneConfig m) = LumatoneConfig (Map.mapWithKey (\b -> Map.mapWithKey (f b)) m)

withChannel :: (Int -> Int) -> LumatoneConfig -> LumatoneConfig
withChannel f = withKeyConf (\kc -> kc { keyConf_chan = f (keyConf_chan kc - 1) `mod` 16 + 1 })

lumatoneIniSettings :: WriteIniSettings
lumatoneIniSettings = WriteIniSettings EqualsKeySeparator

processLtn :: (LumatoneConfig -> LumatoneConfig) -> FilePath -> IO ()
processLtn f fp = processLtn' f fp fp

processLtn' :: (LumatoneConfig -> LumatoneConfig) -> FilePath -> FilePath -> IO ()
processLtn' f fp fp' = do
  r <- readIniFile fp
  case r of
    Left e -> putStrLn e
    Right i ->
      case parseLumatoneConfig i of
        Left e -> putStrLn e
        Right c -> writeIniFileWith lumatoneIniSettings fp' (updateLumatoneConfig (f c) i)

chanToOctave :: (Ord n, Num n) => n -> n
chanToOctave n = if n > 7 then n - 16 - 1 else n - 1

octaveToChan :: (Ord n, Num n) => n -> n
octaveToChan n = if n < 0 then n + 16 + 1 else n + 1

gamut :: Int -> (Int, Int) -> KeyConf -> Double
gamut period (a,b) c = (index - fI a) / (fI (b - a))
  where
    fI = fromIntegral
    octave = chanToOctave (fI (keyConf_chan c))
    index = fI period * octave + fI (keyConf_key c)

dechannelify :: Int -> KeyConf -> KeyConf
dechannelify n kc =
  let rechan (k,c) | 0 <= k && k < 128 = (k, (c `mod` 16) + 1)
                   | k < 0             = rechan (k+n, c-1)
                   | k >= 128          = rechan (k-n, c+1)
  in let (k,c) = rechan (n * chanToOctave (keyConf_chan kc) + keyConf_key kc, 1)
     in kc { keyConf_chan = c, keyConf_key = k }

-- | With the given period, the given starting pitch (relative to that period) for the southwestern point of the keyboard, and the given pair of step sizes horizontally and right-upward,
-- remap the notes on the entire keyboard. Does not affect colours.

regularSteps :: Int -> Int -> (Int, Int) -> LumatoneConfig -> LumatoneConfig
regularSteps period note0 (stepX, stepY) = withBoardKeyConf' keyChanFunction
  where
    computeNote b i =
      let (x,y) = lookupCoords b i
      in note0 + x * stepX + y * stepY
    keyChanFunction b i kc =
      let n = computeNote b i
          c = octaveToChan (n `div` period)
          k = n `mod` period
      in kc { keyConf_chan = c, keyConf_key = k }


autoColour :: Int -- ^ Period (hues are assigned equally spaced modulo this period)
           -> Double -- ^ Saturation of colours [0-1]
           -> Double -- ^ Value (brightness) of colours [0-1]
           -> KeyConf -- ^ Key config to transform
           -> KeyConf
autoColour period saturation value k = k { keyConf_col = noteToColour period (keyConf_key k) saturation value }

noteToColour :: Int -> Int -> Double -> Double -> Colour Double
noteToColour period note saturation value = col
  where hue = 360 * (fromIntegral (note `mod` period) / fromIntegral period)
        col = uncurryRGB (rgbUsingSpace sRGBSpace) (hsv hue saturation value)
