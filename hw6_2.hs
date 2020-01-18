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
before_month_data 1 =0
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
uruu_before_month_data 1 =0
uruu_before_month_data num = uruu_before_month_data (num -1) +uruu_month_data (num-1)

--指定した年の前年までの日数を計算する
total_year_day :: Int ->Int

total_year_day 1584 = 0
total_year_day year = case (uruucheck (year -1)) ==True of
    True -> (total_year_day (year -1)) + 366
    False -> (total_year_day (year -1)) + 365
-- firstyoubi =((totalday `mod` 7) + 6) `mod` 7 --第一引数の年の1/1が何曜日かがわかる　0なら日曜日

youbicalc::Int->Int->Int->Int --1584年1月1日は月曜日である　ということから指定された日の曜日を計算する
youbicalc day month year=case uruucheck year of
    True ->((((total_year_day year)+(uruu_before_month_data month)+day)+6) `mod`7)`mod`7
    False ->((((total_year_day year)+(before_month_data month)+day)+6 )`mod`7)`mod`7

numberToString ::Int->String --'3'を"3  "に変換　"13"を"13 "に変換
numberToString number=case (number<10) of
    True -> (show(number))++" "++" "
    False ->(show(number))++" "

outputDayString7sub ::Int->Int->String
outputDayString7sub startnumber 0="   "
outputDayString7sub startnumber limitnumber=
    (numberToString startnumber)++(outputDayString7sub (startnumber+1) (limitnumber-1))


outputDayString7 :: Int -> Int->Int->String --開始時点の日にち　曜日　その月ひと月が何日かを引数にもって最大7日分をStringとして出力
outputDayString7 startday startyoubi limitday =concat [(replicate (3*(startyoubi+1)) ' ') ,(outputDayString7sub startday limitnumber),amariyohaku ]
    where
        outputdaynumber=7-startyoubi --いくつ数字を出力すればよいかを持っておく 
        limitnumber=case ((startday+outputdaynumber-1)<=limitday) of --いくつ数字を出力すればよいかを持っておく
            True -> outputdaynumber
            False ->(limitday-startday+1) 
        amariyohakunumber=6-(limitnumber+startday-1-startday)
        amariyohaku=case ((limitnumber+startday-1)==limitday)  of
            True->  replicate (amariyohakunumber*3) ' '
            False ->""
outputDayString21 ::Int->Int->IO() --何年何月の何行目か?から数字を21個出力する month=1なら1月2月3月を出力 --(outputDayString7 firstlinethirdstartday firstlinethirdstartyoubi thirdmonthdata)
outputDayString21  month year  =(putStrLn$ monthstring++"\n")>>(putStrLn$ youbistring++"\n")>>(putStrLn$    firstlinestring++"\n") >> (putStrLn $secondlinestring++"\n") >>(putStrLn$ thirdlinestring++"\n") >>(putStrLn $ _4linestring++"\n")>>(putStrLn$ _5linestring++"\n")>>(putStrLn$"\n")
    where
        youbistring="   日 月 火 水 木 金 土       日 月 火 水 木 金 土       日 月 火 水 木 金 土    "
        monthstring=case month of
            1->"   一月                       二月                       三月"
            4->"   四月                       五月                       六月"
            7->"   七月                       八月                       九月"
            10->"   十月                      十一月                     十二月"
        --outputdayString7 におけるlimitdayを求めている
        firstmonthdata=case uruucheck year of
            True -> uruu_month_data month
            False ->month_data month
        secondmonthdata= case uruucheck year of
            True -> uruu_month_data (month+1)
            False -> month_data (month +1)
        thirdmonthdata = case uruucheck year of
            True -> uruu_month_data (month+2)
            False -> month_data (month+2)

        
        firstlinestartday=1
        firstlinestartyoubi=(youbicalc firstlinestartday month year)
        secondlinestartday=firstlinestartday+7-firstlinestartyoubi
        secondlinestartyoubi=(youbicalc secondlinestartday month year)
        thirdlinestartday=secondlinestartday+7
        thirdlinestartyoubi=(youbicalc thirdlinestartday month year)
        _4linestartday=thirdlinestartday+7
        _4linestartyoubi=(youbicalc _4linestartday month year)
        _5linestartday=_4linestartday+7
        _5linestartyoubi=(youbicalc _5linestartday month year)
        
        firstlinesecondstartday=1
        firstlinesecondstartyoubi=(youbicalc firstlinesecondstartday (month+1) year)
        secondlinesecondstartday=firstlinesecondstartday+7-firstlinesecondstartyoubi
        secondlinesecondstartyoubi=(youbicalc secondlinesecondstartday (month+1) year)
        thirdlinesecondstartday=secondlinesecondstartday+7
        thirdlinesecondstartyoubi=(youbicalc thirdlinesecondstartday (month+1)year)
        _4linesecondstartday=thirdlinesecondstartday+7
        _4linesecondstartyoubi=(youbicalc _4linesecondstartday (month+1) year)
        _5linesecondstartday=_4linesecondstartday+7
        _5linesecondstartyoubi=(youbicalc _5linesecondstartday (month+1) year)

        firstlinethirdstartday=1
        firstlinethirdstartyoubi=(youbicalc firstlinethirdstartday (month+2) year)
        secondlinethirdstartday=firstlinethirdstartday+7-firstlinethirdstartyoubi
        secondlinethirdstartyoubi=(youbicalc secondlinethirdstartday (month+2) year)
        thirdlinethirdstartday=secondlinethirdstartday+7
        thirdlinethirdstartyoubi=(youbicalc thirdlinethirdstartday (month+2)year)
        _4linethirdstartday=thirdlinethirdstartday+7
        _4linethirdstartyoubi=(youbicalc _4linethirdstartday (month+2) year)
        _5linethirdstartday=_4linethirdstartday+7
        _5linethirdstartyoubi=(youbicalc _5linethirdstartday (month+2) year)


        firstlinestring=concat[(outputDayString7 firstlinestartday firstlinestartyoubi firstmonthdata),(outputDayString7 firstlinesecondstartday firstlinesecondstartyoubi secondmonthdata),(outputDayString7 firstlinethirdstartday firstlinethirdstartyoubi thirdmonthdata)] 
        secondlinestring=concat[(outputDayString7 secondlinestartday secondlinestartyoubi firstmonthdata),(outputDayString7 secondlinesecondstartday secondlinesecondstartyoubi secondmonthdata),(outputDayString7 secondlinethirdstartday secondlinethirdstartyoubi thirdmonthdata)] 
        thirdlinestring=concat[(outputDayString7 thirdlinestartday thirdlinestartyoubi firstmonthdata),(outputDayString7 thirdlinesecondstartday thirdlinesecondstartyoubi secondmonthdata),(outputDayString7 thirdlinethirdstartday thirdlinethirdstartyoubi thirdmonthdata)] 
        _4linestring=concat[(outputDayString7 _4linestartday _4linestartyoubi firstmonthdata),(outputDayString7 _4linesecondstartday _4linesecondstartyoubi secondmonthdata),(outputDayString7 _4linethirdstartday _4linethirdstartyoubi thirdmonthdata)] 
        _5linestring=concat[(outputDayString7 _5linestartday _5linestartyoubi firstmonthdata),(outputDayString7 _5linesecondstartday _5linesecondstartyoubi secondmonthdata),(outputDayString7 _5linethirdstartday _5linethirdstartyoubi thirdmonthdata)] 

year ::Int->IO()
year number=
    (putStrLn$ show(number)++"年"++"\n")>>(outputDayString21 1 number)>>(outputDayString21 4 number)>>(outputDayString21 7 number)>>(outputDayString21 10 number)


month ::Int->Int->IO()
month month year =(putStrLn$ show(month)++"月"++show(year)++"年"++"\n")>>(putStrLn$(outputDayString7 firstlinestartday firstlinestartyoubi firstmonthdata)++"\n")>>(putStrLn$(outputDayString7 secondlinestartday secondlinestartyoubi firstmonthdata)++"\n")>>(putStrLn$(outputDayString7 thirdlinestartday thirdlinestartyoubi firstmonthdata)++"\n")>>(putStrLn$(outputDayString7 _4linestartday _4linestartyoubi firstmonthdata)++"\n")>>(putStrLn$(outputDayString7 _5linestartday _5linestartyoubi firstmonthdata)++"\n")
    where
        youbistring="   日 月 火 水 木 金 土       日 月 火 水 木 金 土       日 月 火 水 木 金 土    "
        monthstring=case month of
            1->"   一月                       二月                       三月"
            4->"   四月                       五月                       六月"
            7->"   七月                       八月                       九月"
            10->"   十月                      十一月                     十二月"
        --outputdayString7 におけるlimitdayを求めている
        firstmonthdata=case uruucheck year of
            True -> uruu_month_data month
            False ->month_data month
        secondmonthdata= case uruucheck year of
            True -> uruu_month_data (month+1)
            False -> month_data (month +1)
        thirdmonthdata = case uruucheck year of
            True -> uruu_month_data (month+2)
            False -> month_data (month+2)

        
        firstlinestartday=1
        firstlinestartyoubi=(youbicalc firstlinestartday month year)
        secondlinestartday=firstlinestartday+7-firstlinestartyoubi
        secondlinestartyoubi=(youbicalc secondlinestartday month year)
        thirdlinestartday=secondlinestartday+7
        thirdlinestartyoubi=(youbicalc thirdlinestartday month year)
        _4linestartday=thirdlinestartday+7
        _4linestartyoubi=(youbicalc _4linestartday month year)
        _5linestartday=_4linestartday+7
        _5linestartyoubi=(youbicalc _5linestartday month year)
        
        firstlinesecondstartday=1
        firstlinesecondstartyoubi=(youbicalc firstlinesecondstartday (month+1) year)
        secondlinesecondstartday=firstlinesecondstartday+7-firstlinesecondstartyoubi
        secondlinesecondstartyoubi=(youbicalc secondlinesecondstartday (month+1) year)
        thirdlinesecondstartday=secondlinesecondstartday+7
        thirdlinesecondstartyoubi=(youbicalc thirdlinesecondstartday (month+1)year)
        _4linesecondstartday=thirdlinesecondstartday+7
        _4linesecondstartyoubi=(youbicalc _4linesecondstartday (month+1) year)
        _5linesecondstartday=_4linesecondstartday+7
        _5linesecondstartyoubi=(youbicalc _5linesecondstartday (month+1) year)

        firstlinethirdstartday=1
        firstlinethirdstartyoubi=(youbicalc firstlinethirdstartday (month+2) year)
        secondlinethirdstartday=firstlinethirdstartday+7-firstlinethirdstartyoubi
        secondlinethirdstartyoubi=(youbicalc secondlinethirdstartday (month+2) year)
        thirdlinethirdstartday=secondlinethirdstartday+7
        thirdlinethirdstartyoubi=(youbicalc thirdlinethirdstartday (month+2)year)
        _4linethirdstartday=thirdlinethirdstartday+7
        _4linethirdstartyoubi=(youbicalc _4linethirdstartday (month+2) year)
        _5linethirdstartday=_4linethirdstartday+7
        _5linethirdstartyoubi=(youbicalc _5linethirdstartday (month+2) year)


        firstlinestring=concat[(outputDayString7 firstlinestartday firstlinestartyoubi firstmonthdata),(outputDayString7 firstlinesecondstartday firstlinesecondstartyoubi secondmonthdata),(outputDayString7 firstlinethirdstartday firstlinethirdstartyoubi thirdmonthdata)] 
        secondlinestring=concat[(outputDayString7 secondlinestartday secondlinestartyoubi firstmonthdata),(outputDayString7 secondlinesecondstartday secondlinesecondstartyoubi secondmonthdata),(outputDayString7 secondlinethirdstartday secondlinethirdstartyoubi thirdmonthdata)] 
        thirdlinestring=concat[(outputDayString7 thirdlinestartday thirdlinestartyoubi firstmonthdata),(outputDayString7 thirdlinesecondstartday thirdlinesecondstartyoubi secondmonthdata),(outputDayString7 thirdlinethirdstartday thirdlinethirdstartyoubi thirdmonthdata)] 
        _4linestring=concat[(outputDayString7 _4linestartday _4linestartyoubi firstmonthdata),(outputDayString7 _4linesecondstartday _4linesecondstartyoubi secondmonthdata),(outputDayString7 _4linethirdstartday _4linethirdstartyoubi thirdmonthdata)] 
        _5linestring=concat[(outputDayString7 _5linestartday _5linestartyoubi firstmonthdata),(outputDayString7 _5linesecondstartday _5linesecondstartyoubi secondmonthdata),(outputDayString7 _5linethirdstartday _5linethirdstartyoubi thirdmonthdata)] 






        
       

    
