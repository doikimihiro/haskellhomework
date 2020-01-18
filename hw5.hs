kanjiconverter :: Char -> String
kanjiconverter '0'=""
kanjiconverter '1'="壱"
kanjiconverter '2'="弐"
kanjiconverter '3'="参"
kanjiconverter '4'="四"
kanjiconverter '5'="五"
kanjiconverter '6'="六"
kanjiconverter '7'="七"
kanjiconverter '8'="八"
kanjiconverter '9'="九"


ketaconverter :: Int ->String
ketaconverter 10 ="拾"
ketaconverter 100 = "百"
ketaconverter 1000 ="千"
ketaconverter 10000="万"
ketaconverter 100000000="億"

keta1converter :: String ->String 
keta1converter str = case head str of
    '0' -> ""
    _ -> kanjiconverter (head str)

keta2converter :: String -> String
keta2converter str = case head str of
    '0' -> (keta1converter (tail str))
    '1' -> ketaconverter 10 ++ keta1converter (tail str)
    _ -> kanjiconverter (head str) ++ ketaconverter 10 ++ keta1converter (tail str)

keta3converter :: String -> String
keta3converter str = case head str of
    '0' -> (keta2converter (tail str))
    '1' -> ketaconverter 100 ++ keta2converter (tail str)
    _ -> kanjiconverter (head str) ++  ketaconverter 100 ++ keta2converter (tail str)

keta4converter :: String ->String 
keta4converter str =case head str of
    '0' -> (keta3converter (tail str))
    '1' -> ketaconverter 1000 ++ keta3converter (tail str )
    _ -> kanjiconverter (head str) ++  ketaconverter 1000 ++ keta3converter (tail str)

keta5converter :: String ->String 
keta5converter str =case head str of
    '0' -> (keta4converter (tail str))
    
    _ -> kanjiconverter (head str) ++  ketaconverter 10000 ++ keta4converter (tail str)
   
    
--ここから(10万以降)は例外処理をする    
keta6converter :: String ->String 
keta6converter str =case head str of
    '0' -> keta5converter (tail str)
    
    _ -> case (head (tail str)) of
        '0' ->  kanjiconverter (head str) ++  ketaconverter 10 ++ketaconverter 10000++ keta5converter (tail str ) 
        _ ->  kanjiconverter (head str) ++  ketaconverter 10 ++ keta5converter (tail str ) 

--頭3要素をtake 3 strで切り出して、9999以下は4桁用の関数を呼んで結合する バグの原因か?
keta7converter :: String ->String 
keta7converter str =case head str of 
    '0' -> (keta6converter (tail str))
    _ -> keta3converter (take 3 str) ++ketaconverter 10000++ keta4converter (drop 3 str) 


keta8converter :: String ->String --バグの原因か?

keta8converter str =case head str of 
    '0' -> (keta7converter (tail str))
    _ -> keta4converter (take 4 str) ++"万"++ keta4converter (drop 4 str) 


--keta9converter :: String ->String 
--keta9converter str =case head str of
  --  '0' -> keta8converter (tail str)
    --_ ->kanjiconverter (head str) ++"億"++(keta8converter (drop 1 str))
    


--keta10converter :: String ->String
--keta10converter str = case head str of
  --  '0' -> keta9converter (tail str)
   -- _ -> keta2converter (take 2 str)++"億" ++keta8converter (drop 2 str)
    
convert0 :: String -> String
convert0 [] = []
convert0 str = case length str of
    1 ->case head str of
        '0' -> "零"
        _ -> keta1converter str
    2 ->keta2converter str
    3 -> keta3converter str
    4 ->keta4converter str 
    5 -> keta5converter str 
    6 -> keta6converter str 
    7 -> keta7converter str 
    8 ->keta8converter str 
    9 ->headchar++keta8converter (tail str)
        where 
            headchar=(kanjiconverter (head str))++"億"     
    10 ->"拾億"   


convert1 str = case str of
    [] -> putStrLn ""
    _ -> putStrLn $ "金" ++ (convert0 str) ++ "円"


convert integer = convert1 (show(integer))

