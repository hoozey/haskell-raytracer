import Codec.Picture
import Codec.Picture.Types
import Data.Vector.V3
import Data.Vector.Class
import Data.Maybe

data Color = Color Double Double Double deriving Show
data Sphere = Sphere { center :: Vector3, radius :: Double, diffuse :: Color, refl :: Double } deriving Show
data Light = Light { l_position :: Vector3, l_brightness :: Double } deriving Show
data Ray = Ray { position :: Vector3, direction :: Vector3 } deriving Show
data Intersection = Intersection { i_position :: Vector3, i_ray :: Ray, i_sphere :: Sphere, i_distance :: Double } deriving Show
type Scene = ([Sphere],[Light],Int,Int)

main :: IO ()
main = do
  createImage scene "./output.png"

createImage :: Scene -> String -> IO ()
createImage (ss, ls, w, h) path = writePng path $ generateImage (renderPixel (ss, ls, w, h)) w h

renderPixel :: Scene -> Int -> Int -> PixelRGB8
renderPixel s x y = color2pixel (renderColor s x y)

renderColor :: Scene -> Int -> Int -> Color
renderColor (ss, ls, w, h) x y = maybe ambientColor (colorAt 4 ls ss) (intersection' (primaryRay (pixelLocation w h x y)) ss)

color2pixel :: Color -> PixelRGB8
color2pixel (Color r g b) = PixelRGB8 (f r) (f g) (f b)
  where clamp x = (max 0) $ (min 1) x
        f x = round ((clamp x) * 255)

pixelLocation :: Int -> Int -> Int -> Int -> Vector3
pixelLocation imageWidth imageHeight i j = Vector3 (i' - w'/2) ((-1)*(j' - h'/2)) 0 where
  i' = fromIntegral i
  j' = fromIntegral j
  w' = fromIntegral imageWidth
  h' = fromIntegral imageHeight

primaryRay :: Vector3 -> Ray
primaryRay pixelLocation = Ray (pixelLocation + Vector3 0 0 1000) (Vector3 0 0 (-1))

ambientColor = Color 0 0 0

scene :: Scene
scene =
  let
      -- sphere colors
      k = 0.1
      red = Color k 0 0
      green = Color 0 k 0
      blue = Color 0 0 k
      cyan = Color 0 k k
      magenta = Color k 0 k
      yellow = Color k k 0
      white = Color k k k
      colors = [cyan, magenta, yellow, red, blue, green, white]

      -- sphere positions
      v x y z = Vector3 x y z
      cmy = map (*266) [v 0 1 0, v 0.86 (-0.5) 0, v (-0.86) (-0.5) 0]
      rgb = map (*266) [v 0 (-1) 0, v 0.86 0.5 0, v (-0.86) 0.5 0]
      wht = [v 0 0 0]
      positions = cmy ++ rgb ++ wht

      r = 133
      rf = 0.5

      spheres = map (\(c,p) -> Sphere p r c rf) (zip colors positions)
      lights = [Light (Vector3 (-100) 100 (200)) 15, Light (Vector3 (100) (-50) (800)) 10]

  in (spheres, lights, 800, 800)

--closest intersection if there is one
intersection :: Ray -> Sphere -> Maybe Intersection
intersection ray sphere =
  let l = vnormalise $ direction ray
      o = position ray
      c = center sphere
      r = radius sphere
      otherPart = 0-(l `vdot` (o-c))
      partUnderSqrt = (l `vdot` (o-c))**2 - (vmag (o-c))**2 + r**2
      ds | partUnderSqrt > 0  = [otherPart + (sqrt partUnderSqrt), otherPart - (sqrt partUnderSqrt)]
         | partUnderSqrt == 0 = [otherPart]
         | otherwise          = []
      threshold = 1
  in case (filter (>threshold) ds) of
    []  -> Nothing
    ds' -> Just $ (\d -> let pos = o + l|*d in Intersection pos ray sphere d) (minimum ds')

intersection' :: Ray -> [Sphere] -> Maybe Intersection
intersection' ray spheres =
  let is = catMaybes $ map (intersection ray) spheres
      closer :: Intersection -> Intersection -> Intersection
      closer a b = if i_distance a <= i_distance b then a else b
      closest :: [Intersection] -> Intersection
      closest is = foldl1 closer is
  in case is of
    [] -> Nothing
    _ -> Just $ closest is

-- whether the light is blocked by another sphere
shadow :: Intersection -> Light -> [Sphere] -> Bool
shadow x l ss =
  let lightRay :: Intersection -> Light -> Ray
      lightRay (Intersection ipos _ _ _) (Light lpos _) = Ray lpos (ipos - lpos)
      lr = lightRay x l
      mlx = (intersection lr (i_sphere x))
      mox = intersection' lr ss
  in case mox of
    (Just ox) -> case mlx of
      Just lx -> i_distance lx - i_distance ox > 0.1
      Nothing -> False
    Nothing -> False

colorAt :: Int -> [Light] -> [Sphere] -> Intersection -> Color
colorAt 0 _ _ _ = ambientColor
colorAt bouncesLeft ls ss x =
  let reflect :: Vector3 -> Vector3 -> Vector3
      reflect v n = v - 2 * (v `vdot` n) *| n

      normal :: Intersection -> Vector3
      normal (Intersection pos _ sphere _) = vnormalise (pos - (center sphere))

      --direction toward light from sphere
      lightVector :: Intersection -> Light -> Vector3
      lightVector (Intersection ipos _ _ _) (Light lpos _) = vnormalise $ lpos - ipos

      cosTheta :: Intersection -> Light -> Double
      cosTheta i l = (lightVector i l) `vdot` (normal i)

      brightness :: Intersection -> Light -> [Sphere] -> Double
      brightness x l ss = if (shadow x l ss) then 0 else (l_brightness l) * ((max 0)(cosTheta x l))

      brightness' :: [Light] -> [Sphere] -> Intersection -> Double
      brightness' ls ss x = foldl (+) 0 (map (\l -> brightness x l ss) ls)

      lighten :: Color -> Double -> Color
      lighten (Color r g b) k = (Color (f r) (f g) (f b)) where f x = x + (k*x)

      mix :: Color -> Color -> Double -> Color
      mix (Color dr dg db) (Color rr rg rb) r = Color (d*dr + r*rr) (d*dg + r*rg) (d*db + r*rb) where d = (1-r)

      s = i_sphere x
      d = lighten (diffuse s) (brightness' ls ss x)
      rr = Ray (i_position x) (reflect (direction (i_ray x)) (normal x))
      mx' = intersection' rr ss
      r = case mx' of
        Just x' -> colorAt (bouncesLeft-1) ls ss x'
        Nothing -> ambientColor
  in mix d r (refl s)
