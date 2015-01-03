module Reader where

import Data.List (elemIndex)                  
import Data.Bits (xor)
import Data.Maybe

data SourceCode = SourceCode { raw        :: String
                             , rows       :: [String]
                             , row_count  :: Int
                             , src_length :: Int
                             } deriving (Show)

makeSourceCode raw = 
        let src_lines = lines raw
            lines_cnt = length src_lines
        in  SourceCode raw src_lines lines_cnt (length raw)

data Position = Position { row :: Int, col :: Int } deriving (Show, Eq)

data Direction = Direction { dcol :: Int, drow :: Int } deriving (Show, Eq)

p <~ d = Position (row p + drow d) (col p + dcol d)

elemAt i l =
        if (i < length l) && (i >= 0)
                then Just (l !! i)
                else Nothing 

src @@ pos =
        elemAt (row pos) (rows src) >>= elemAt (col pos)

direction_table = 
        [Direction (round$cos v) (-(round$sin v)) | v <- map (*(pi/2)) [0..7]]

direction_literal = "→↑←↓"
mirror_litetal = "/\\"

detectDirection char =
       elemIndex char direction_literal >>= \i -> Just $ direction_table !! i

reflect char dir =
        let mask = if char == '/' then 1 else 3
        in  elemIndex dir direction_table
            >>= \i -> Just $ direction_table !! (xor mask i)

updateDirection char dir 
        | elem char mirror_litetal = reflect char dir
        | otherwise = Nothing

shouldPrint char = 
        not $ elem char mirror_litetal || elem char direction_literal

findStartInLine line lineno =
        let fi = \i -> detectDirection (line !! i) 
                       >>= \x -> Just $ (Position lineno i, x)
        in  catMaybes $ map fi [0..(length line)-1]

findStart src = 
        let limit = row_count src - 1
            src_rows = rows src
            fn = \i -> findStartInLine (src_rows !! i) i
            starts = foldl1 (++) $ map fn [0..limit]
        in  if null starts 
                then Nothing
                else Just starts

readThrough src pos dir =
        _readThrough src (pos<~dir) dir

_readThrough src pos dir =
        case src@@pos of
                Just curr ->
                        let next_dir = fromMaybe dir $ updateDirection curr dir
                            next_pos = pos<~next_dir
                            remains = _readThrough src next_pos next_dir
                        in if shouldPrint curr 
                                   then curr:remains
                                   else remains
                Nothing   -> []
        
