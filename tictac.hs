-- X goes first. X winning is one point; O winning is -1.

xwin = 1
owin = -1
ewin = 0

valuemarker m
    | m == X = xwin
    | m == O = owin
    | m == E = ewin

othermarker m
    | m == X = O
    | m == O = X
    | m == E = E

data Marker = X | O | E deriving (Show, Eq, Ord)
type Position = [Marker]

data TreeNode a = Branch a [TreeNode a] | EmptyNode

instance Show a => Show (TreeNode a) where
    show EmptyNode = "EmptyNode"
    show (Branch a children) = "Branch " ++ show a ++ " " ++ show children

treeApply x = Branch x (map treeApply $ next x)
gametree = treeApply empty

prune depth tree
    | depth <= 0 = case tree of
        Branch value children -> Branch value []
        EmptyNode -> EmptyNode
    | otherwise = case tree of
        Branch value children -> Branch value $ map (prune (depth-1)) children
        EmptyNode -> EmptyNode

eval marker tree = case tree of
    Branch value children -> if whowon /= E then valuemarker whowon else maximum $ map (eval (othermarker marker)) children
                                where whowon = won value
    EmptyNode -> 0

p = concat [[X,O,O],[O,X,X],[E,E,E]]
empty = concat [[E,E,E],[E,E,E],[E,E,E]]

countMarker :: Position -> Marker -> Int
countMarker pos m = length $ filter (==m) pos

possibilities :: Position -> Marker -> [Position]
possibilities (h:r) m = if h == E then (m:r):(map (h:) (possibilities r m)) else map (h:) (possibilities r m)
possibilities [] _ = []

next :: Position -> [Position]
next pos = possibilities pos turn
    where numo = countMarker pos O
          numx = countMarker pos X
          turn = if numx > numo then O else X

weight :: Marker -> Position -> Int
weight marker position = static marker 5 position

static :: Marker -> Int -> Position -> Int
static mymarker depth pos
    | depth <= 0 = 0
    | winning == X = xwin
    | winning == O = owin
    | otherwise = maximum $ map (static mymarker (depth-1)) (next pos)
    where winning = won pos

won :: Position -> Marker
won pos = if countMarker pos X < 3 then E else 
            case pos of
                [X, X, X, _, _, _, _, _, _] -> X
                [_, _, _, X, X, X, _, _, _] -> X
                [_, _, _, _, _, _, X, X, X] -> X
                [X, _, _, X, _, _, X, _, _] -> X
                [_, X, _, _, X, _, _, X, _] -> X
                [_, _, X, _, _, X, _, _, X] -> X
                [X, _, _, _, X, _, _, _, X] -> X
                [_, _, X, _, X, _, X, _, _] -> X

                [O, O, O, _, _, _, _, _, _] -> O
                [_, _, _, O, O, O, _, _, _] -> O
                [_, _, _, _, _, _, O, O, O] -> O
                [O, _, _, O, _, _, O, _, _] -> O
                [_, O, _, _, O, _, _, O, _] -> O
                [_, _, O, _, _, O, _, _, O] -> O
                [O, _, _, _, O, _, _, _, O] -> O
                [_, _, O, _, O, _, O, _, _] -> O
                
                otherwise -> E

--main = putStrLn $ show $ map printPosition (next p)