import Data.List

lookAndSay = concat . concatMap (\g -> [show $ length g, take 1 g]) . group
getSolution n = length $ (drop n $ iterate lookAndSay "1113222113") !! 0
main = mapM_ (print . getSolution) [40, 50]
