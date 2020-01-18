import Data.Char

type Position =(Int,Int)

getInteger ::String ->Int->(Int,String)
getInteger [] x =(x,[])
getInteger (c:cs) x|isDigit c=getInteger cs (10*x+digitToInt c)
                   |otherwise =(x ,cs)

makePosition ::String ->Position
makePosition s1=(x,y)
         where 
            (x,s2)=getInteger s1 0
            (y ,s3)=getInteger s2 0

valid ::Position ->Bool
valid (x,y) =(0<=x&&x<=2)&&(0<=y&&y<=2)

getPosition :: IO Position 
getPosition = do --putStr "please input: "
                 str <-getLine
                 let (x,y) =makePosition str
                 validate (x,y)
    where validate(x,y)|valid (x,y) =return (x,y)
                       |otherwise =getPosition 

getPositionOutput =do
             x <- getPosition
             print x


turncheck ::Int->Bool --偶数なら先攻でTrueを返す
turncheck turn=case turn `mod` 2 of
    0 -> True
    1 ->False

fieldconverter ::Position ->Int
fieldconverter pos=case pos of
    (0,0)->0
    (0,1)->1
    (0,2)->2
    (1,0)->3
    (1,1)->4
    (1,2)->5
    (2,0)->6
    (2,1)->7
    (2,2)->8

allfield=[[2,2,2,2,2,2,2,2,2]]

changefield ::Int->Int->[Int]->[Int]
changefield newstonenumber turnnumber prefield=
    case turnnumber`mod`2 of
        0->(take (newstonenumber) prefield ) ++[0]++(drop (newstonenumber+1) prefield)
        1->(take (newstonenumber) prefield ) ++[1]++(drop (newstonenumber+1) prefield)



-- 0なら先攻の石　1なら後攻の石　2なら空
generatefield ::Int->Position->[Int] --turn position 盤面
generatefield turnnumber pos =(changefield newstonenumber turnnumber prefield)
    where 
        prefield=allfield!!turnnumber
        newfield=[]
        newstonenumber=fieldconverter pos

generatefield2 ::[Int]->[[Int]]
generatefield2 newfield=allfield++[newfield]        
 
--ターン数はallfieldの長さから取得する
playerturn ()=do    
            pos <-getPosition
            print (generatefield2 (generatefield (length allfield) pos)) 

--turnplayer :IO()
--turnplayer=


--gamestart :: IO()
--gamestart = 