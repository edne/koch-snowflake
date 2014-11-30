import Graphics.UI.GLUT

main :: IO ()
main = do
    getArgsAndInitialize
    createWindow "Koch Snowflake"
    displayCallback $= display
    mainLoop
 
display :: DisplayCallback
display = do 
    clear [ColorBuffer]
    renderPrimitive LineStrip $ mapM_ drawPoint points
    flush
    where
        drawPoint (x,y) = vertex (Vertex3 x y 0)
 
-- rule
data Rule = Rule {
    fraction :: GLfloat,
    angles :: [GLfloat]
}

rule :: Rule
--rule = Rule 3 [0, pi/2, 0, -pi/2, 0]
rule = Rule 3 [0, pi/3, -pi/3, 0]
--

-- points
type Point = (GLfloat, GLfloat)

dist :: Point -> Point -> GLfloat
dist (x1,y1) (x2,y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

angDist :: Point -> Point -> GLfloat
angDist (x1,y1) (x2,y2) = atan2 (y2-y1) (x2-x1)

points :: [Point]
points = koch 4 [(-1, 0), (1, 0)]
--

-- recursively generate
koch :: Int -> [Point] -> [Point]
koch 0 ps = ps
koch n ps = koch (n-1) $ splitAll ps

-- split every segment
splitAll :: [Point] -> [Point]
splitAll [] = []
splitAll (p:[]) = [p]
splitAll (p:ps) = (split p $ head ps) ++ splitAll ps

-- split a segment
split :: Point -> Point -> [Point]
split a b = generate [a] angs l
    where
        angs = [ang + angDist a b | ang<-angles rule]
        l = (dist a b) / (fraction rule)

-- move from a point
generate :: [Point] -> [GLfloat] -> GLfloat -> [Point]
generate ps [] l = ps
generate ps (ang:angs) l = ps ++ generate [move (last ps) l ang] angs l

-- move by 1 step
move :: Point -> GLfloat -> GLfloat -> Point
move (x,y) rho theta = (x + rho*cos(theta), y + rho*sin(theta))
