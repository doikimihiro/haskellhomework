
polyplussub::[Int]-> [Int] -> [Int]
polyplussub as bs =
    case as of y
        [] -> case bs of 
            [] -> []
            _ -> bs
        _ ->case bs of
            [] -> as
            _ -> (head as + head bs) : polyplussub (tail as) (tail bs)



--リスト反転後の先頭の0をすべて削除
erase0 ::[Int] ->[Int]
erase0 array = case head  array of
    0 -> erase0 (tail array)
    _ -> array

polyplus :: [Int] ->[Int] -> [Int]
polyplus as bs = reverse (erase0 (reverse (polyplussub as bs)))


    
polymultsub0 :: Int->[Int]->[Int]
polymultsub0 num [] = []
polymultsub0 num bs = map (\x -> x * num) bs

polymultsub1 :: Int ->[Int]->[Int]
polymultsub1 index bs =
    
    case index of
        0 -> bs
        _ -> 0 : polymultsub1 (index - 1) bs

polymultsub2 :: Int -> Int -> [Int]->[Int]
polymultsub2 num index [] = polymultsub1 index (polymultsub0 num [])
polymultsub2 num index bs = polymultsub1 index (polymultsub0 num bs)        

polymultsub :: [Int] ->Int-> [Int] -> [Int]
polymultsub as index bs = case as of
        [] -> []
        _ -> polyplus (polymultsub2 (head as) index bs) (polymultsub (tail as) (index+1) bs)
    
polymult ::[Int] ->[Int] ->[Int]
polymult as bs = polymultsub as 0 bs
    

