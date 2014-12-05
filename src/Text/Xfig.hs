{-# LANGUAGE OverloadedStrings #-}

module Text.Xfig where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf

data PointLine = PointLine { pointline_pairs :: [ (Int,Int) ] }
               deriving (Show,Ord,Eq)

data Spline = Spline { spline_object_code :: Int
                     , spline_sub_type :: Int
                     , spline_line_style :: Int
                     , spline_thickness :: Int
                     , spline_pen_color :: Int
                     , spline_fill_color :: Int
                     , spline_depth :: Int
                     , spline_pen_style :: Int
                     , spline_area_fill :: Int
                     , spline_style_val :: Float
                     , spline_cap_style :: Int
                     , spline_forward_arrow :: Int
                     , spline_backward_arrow :: Int
                     , spline_npoints :: Int 
                     -- , spline_forward_arrow_line :: Maybe ArrowLine
                     -- , spline_backward_arrow_line :: Maybe ArrowLine
                     , spline_point_line :: PointLine
                     , spline_control_point_line :: [Int]
                     }
            deriving (Show,Eq,Ord)

pair :: Parser (Int,Int)
pair = do
    x <- decimal
    skipSpace
    y <- decimal
    skipSpace
    return (x,y)

spline :: Parser Spline
spline = do
    string "# spline" 
    skipSpace
    object_code <- decimal
    skipSpace
    sub_type <- decimal
    skipSpace
    line_style <- decimal
    skipSpace
    thickness <- decimal
    skipSpace
    pen_color <- decimal
    skipSpace
    fill_color <- decimal
    skipSpace
    depth <- decimal
    skipSpace
    pen_style <- decimal
    skipSpace
    area_fill <- signed decimal
    skipSpace
    style_val <- double
    skipSpace
    cap_style <- decimal
    skipSpace
    forward_arrow <- decimal
    skipSpace
    backward_arrow <- decimal
    skipSpace
    npoints <- decimal
    skipSpace
    pairs <- replicateM npoints pair
    skipSpace
    cpts <- replicateM npoints (signed decimal <* skipSpace)
    return Spline { spline_object_code = object_code
                  , spline_sub_type    = sub_type
                  , spline_line_style  = line_style
                  , spline_thickness   = thickness
                  , spline_pen_color   = pen_color
                  , spline_fill_color  = fill_color
                  , spline_depth       = depth
                  , spline_pen_style   = pen_style
                  , spline_area_fill   = area_fill
                  , spline_style_val   = realToFrac style_val
                  , spline_cap_style   = cap_style
                  , spline_forward_arrow  = forward_arrow
                  , spline_backward_arrow = backward_arrow
                  , spline_npoints     = npoints 
                  , spline_point_line  = PointLine pairs
                  , spline_control_point_line = cpts
                  }

