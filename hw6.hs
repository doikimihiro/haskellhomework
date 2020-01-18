uruucheck :: Int -> Bool
uruucheck number = case number `mod` 400 == 0 of
    True -> True
    False -> case number `mod` 4 ==0 && number `mod` 100 /= 0 of
        True -> True
        False -> False

month_data ::Int ->Int 
month_data 1 =31
month_data 2 =28
month_data 3 =31
month_data 4 =30
month_data 5 = 31
month_data 6= 30
month_data 7=31
month_data 8 =31
month_data 9 =30
month_data 10 = 31
month_data 11 =30
month_data 12 =31

before_month_data ::Int->Int
before_month_data 2 =31
before_month_data num = before_month_data (num -1) +month_data (num-1)

uruu_month_data ::Int->Int
uruu_month_data 1 =31
uruu_month_data 2 =29 --閏年
uruu_month_data 3 =31
uruu_month_data 4 =30
uruu_month_data 5 = 31
uruu_month_data 6= 30
uruu_month_data 7=31
uruu_month_data 8 =31
uruu_month_data 9 =30
uruu_month_data 10 = 31
uruu_month_data 11 =30
uruu_month_data 12 =31

uruu_before_month_data ::Int->Int
uruu_before_month_data 2 =31
uruu_before_month_data num = uruu_before_month_data (num -1) +uruu_month_data (num-1)

-- 引数に数字を与えると7連続で表示し、最後に改行する関数 ある数を超えた場合はそこでうちきる(31日など)
output7days :: Int-> Int-> String
output7days initnum limit =case (initnum+6) <= limit of
        True -> (show(initnum)) ++  " " ++  (show(initnum+1)) ++ " " ++ (show(initnum+2)) ++ " " ++ (show(initnum+3)) ++ " " ++  (show (initnum + 4 )) ++ " " ++ (show(initnum+5)) ++ " " ++ (show(initnum+6))
        False -> case (initnum+5) <= limit of
            True ->(show(initnum))++" "++ (show(initnum+1))++" "++(show(initnum+2)) ++" "++(show(initnum+3))++" "++(show(initnum+4))++" "++(show(initnum+5))
            False -> case (initnum+4) <= limit of 
                True -> (show(initnum))++" "++ (show(initnum+1))++" "++(show(initnum+2)) ++" "++(show(initnum+3))++" "++(show(initnum+4))
                False -> case (initnum + 3) <= limit of
                    True ->(show(initnum))++" "++ (show(initnum+1))++" "++(show(initnum+2)) ++" "++(show(initnum+3))
                    False -> case (initnum +2) <= limit of
                        True ->(show(initnum))++" "++ (show(initnum+1))++" "++(show(initnum+2))
                        False -> case (initnum +1)<= limit  of
                            True ->(show(initnum))++" "++ (show(initnum+1))
                            False -> case initnum <=limit of
                                True -> (show(initnum))
                                False -> ""

-- カレンダー一行目の空白を表現する関数
output7daysfirstline :: Int -> String
-- 第一引数のIntは1/1が何曜日か？をしめす
--月曜日を1火曜日を2と対応させる 日曜日は0
output7daysfirstline youbi = case youbi of
    0 -> output7days 1 31
    1 -> "  "++ output7days 1 6
    2 -> "    "++ output7days 1 5
    3 -> "      "++ output7days 1 4
    4 -> "        "++ output7days 1 3
    5 -> "          "++ output7days 1 2
    6 -> "            "++ output7days 1 1

--1/1の曜日,その月の日数を引数にn/mを全て出力する
outputdays ::Int->Int->IO()
outputdays youbi limit = putStrLn $ "日 月 火 水 木 金 土\n"++output7daysfirstline youbi ++ "\n"  ++ output7days (7 - youbi + 1 ) limit ++ "\n"   ++output7days (((7-youbi)+1)+7) limit ++ "\n" ++output7days (((7-youbi)+1)+14) limit ++ "\n"++output7days (((7-youbi)+1)+21) limit ++ "\n"++output7days (((7-youbi)+1)+28) limit ++ "\n"

--指定した年の前年までの日数を計算する
total_year_day :: Int ->Int
total_year_day 1583 =0
total_year_day 1584 = 365  --西暦1年は365日ある
total_year_day year = case (uruucheck (year -1)) ==True of
    True -> (total_year_day (year -1)) + 366
    False -> (total_year_day (year -1)) + 365



--閏年ではない年のカレンダー
outputyear ::Int ->IO()
outputyear num =(putStrLn $ (show(num))++"年") >>(putStrLn $  "1月" ++ "\n" )>> (outputdays firstyoubi (month_data 1))  >> (putStrLn $ "2月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 2 )`mod`7) (month_data 2)) >> (putStrLn $ "3月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 3 )`mod`7) (month_data 3))>> (putStrLn $ "4月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 4 )`mod`7) (month_data 4))>> (putStrLn $ "5月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 5 )`mod`7) (month_data 5))>> (putStrLn $ "6月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 6 )`mod`7) (month_data 6))>> (putStrLn $ "7月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 7 )`mod`7) (month_data 7))>> (putStrLn $ "8月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 8 )`mod`7) (month_data 8))>> (putStrLn $ "9月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 9 )`mod`7) (month_data 9))>> (putStrLn $ "10月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 10 )`mod`7) (month_data 10))>> (putStrLn $ "11月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 11 )`mod`7) (month_data 11))>> (putStrLn $ "12月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 12 )`mod`7) (month_data 12)) 
--
    where 
        totalday = total_year_day num --ここで第一引数の前年までの経過日数がわかる
        firstyoubi =((totalday `mod` 7) + 6) `mod` 7 --第一引数の年の1/1が何曜日かがわかる　0なら日曜日
-- (putStrLn $ "2月" ++ "\n")>>(outputdays ((firstyoubi + before_month_data 2 )`mod`7) (month_data 2))
--閏年の一月分のカレンダーを出力
sub_uruuoutputyear :: Int->Int->IO()
sub_uruuoutputyear month year =(putStrLn $ (show(month))++"月" ++ "\n")>>(outputdays ((firstyoubi + uruu_before_month_data month )`mod`7) (uruu_month_data month))
        where
            totalday = total_year_day year --ここで第一引数の前年までの経過日数がわかる
            firstyoubi =((totalday `mod` 7) + 6) `mod` 7 --第一引数の年の1/1が何曜日かがわかる　0なら日曜日


uruuoutputyear ::Int ->IO()
uruuoutputyear num =(putStrLn $ (show(num))++"年") >>(putStrLn $  "1月" ++ "\n" )>> (outputdays firstyoubi (uruu_month_data 1)) >>(sub_uruuoutputyear 2 num)>>(sub_uruuoutputyear 3 num)>>(sub_uruuoutputyear 4 num)>>(sub_uruuoutputyear 5 num)>>(sub_uruuoutputyear 6 num)>>(sub_uruuoutputyear 7 num)>>(sub_uruuoutputyear 8 num)>>(sub_uruuoutputyear 9 num)>>(sub_uruuoutputyear 10 num)>>(sub_uruuoutputyear 11 num)>>(sub_uruuoutputyear 12 num)


    where
        totalday = total_year_day num --ここで第一引数の前年までの経過日数がわかる
        firstyoubi =((totalday `mod` 7) + 6) `mod` 7 --第一引数の年の1/1が何曜日かがわかる　0なら日曜日

year :: Int->IO()
year num =(putStrLn $ (show(num))++"年"++"\n")>> case uruucheck num of
    True -> uruuoutputyear num
    False -> outputyear num


month :: Int->Int->IO()
month mo ye =(putStrLn $ (show(mo))++ "月" ++" "++ (show(ye))++"年"++"\n")>>(outputdays ((firstyoubi + uruu_before_month_data mo )`mod`7) (uruu_month_data mo))
    
    where
        totalday = total_year_day ye --ここで第一引数の前年までの経過日数がわかる
        firstyoubi =((totalday `mod` 7) + 6) `mod` 7 --第一引数の年の1/1が何曜日かがわかる　0なら日曜日




