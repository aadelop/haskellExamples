type Height = Int
type Width = Int

data Picture = Picture {
    height :: Height,
    width :: Width,
    pixels :: [[Char]]
} deriving (Show)

 
x :: Picture
y :: Picture
a :: Picture
b :: Picture
(x,y,a,b) = (pixel 'x',pixel 'y',pixel 'a',pixel 'b')

type Day = Int -- Suponga que está entre 1 y 31
type Year = Int -- Suponga que es positivo
data Month = Enero
    | Febrero
    | Marzo
    | Abril
    | Mayo
    | Junio
    | Julio
    | Agosto
    | Septiembre
    | Octubre
    | Noviembre
    | Diciembre
    deriving (Show,Eq,Ord,Enum)

data DayName = Domingo
    | Lunes
    | Martes
    | Miercoles
    | Jueves
    | Viernes
    | Sabado
    deriving (Show,Eq,Ord,Enum)


pixel :: Char -> Picture
pixel c = Picture{
        height = 1,
        width = 1,
        pixels = [[c]]
        } 

above :: Picture -> Picture -> Picture
above p0 p1 
    | wp0 == wp1 = Picture {
                    height = hp0 + hp1,
                    width = wp0,
                    pixels = pp0 ++ pp1
                }
    |otherwise = error "error, difiere weight"
    where  (wp0, wp1) = (width p0, width p1)
           (hp0, hp1) = (height p0, height p1)
           (pp0, pp1) = (pixels p0, pixels p1) 

beside :: Picture -> Picture -> Picture
beside p0 p1
    | hp0 == hp1 = Picture {
                    height = hp0,
                    width = wp0 + wp1,
                    pixels =zipWith (++) pp0  pp1
                }
    |otherwise = error "error, difiere height"
    where  (wp0, wp1) = (width p0, width p1)
           (hp0, hp1) = (height p0, height p1)
           (pp0, pp1) = (pixels p0, pixels p1)

toString :: Picture -> String 
toString  =  (unlines .pixels) 


stack:: [Picture] -> Picture
stack = foldr1 above

--stack [x] = x
--stack (x:xs) = above x (stack xs) 

spread :: [Picture] -> Picture
spread = foldr1 beside

row :: String -> Picture
row   = (spread . (map pixel)) 

blank::(Height, Width) -> Picture
blank = uncurry buildBlank 
         where stackB h = stack (replicate h (pixel ' '))
               buildBlank h w = spread (replicate w (stackB h))

--stackWith :: Height -> [Picture] -> Picture 
--stackWith h ps = stack (head ps : (tail (map (above (conWhite h)) ps)))
--                 where conWhite h = blank (h,width (head ps))

stackWith :: Height -> [Picture] -> Picture 
stackWith h = (stack . (map (buildBlank h)))
               where buildBlank h ps = above ps (blank (h,width ps))


stackWith' :: Height -> [Picture] -> Picture 
stackWith' h = foldr1 (\ps -> above (above ps (blank (h, width ps))))

spreadWith :: Height -> [Picture] -> Picture
spreadWith w =(spread . (map (buildBlank w )))
               where buildBlank w hs = beside hs (blank(height hs,w))


tile :: [[Picture]] -> Picture
tile = (spread . (map stack)) 

tileWith::(Height, Width) -> [[Picture]] -> Picture
tileWith (h,w) = ((spreadWith w). (map (stackWith h)))


{-CALENDAR TYPE-}

leap :: Year -> Bool
leap y = if (((mod y 4 ==0) && 
            (mod y 100 /= 0)) ||
            (mod y 400 == 0))         
         then True
         else False

mlengths :: Year -> [Day]
mlengths y = [31,feb,31,30,31,30,31,31,30,31,30,31]
        where feb | leap y = 29
                      | otherwise = 28

jan1 :: Year -> DayName
jan1 y =  toEnum (((numDays y) `mod` 7))
        where numDays y  = (y-1) * 365 + numLeap y + 1 
              numLeap y  = length (filter leap [1..y-1])

mtotals :: Year -> [Int]
mtotals y =scanl (+) (fromEnum (jan1 y)) (mlengths y)


