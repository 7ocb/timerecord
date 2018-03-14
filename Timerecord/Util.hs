module Timerecord.Util
    (mapBy2)

where

mapBy2 :: (a -> a -> b) -> [a] -> [b]
mapBy2 f xs = map (call f) $ zip xs $ tail xs
              where call f (x, y) = f x y  