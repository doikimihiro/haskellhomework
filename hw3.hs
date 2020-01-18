


delete ::Eq a=>a->[a]->[a]
delete num []=[]
delete num (x:xs) = if x==num then xs else x:delete num xs

deleteAll::Eq a=>a->[a]->[a]
deleteAll num []=[]
deleteAll num (x:xs) = if x==num then deleteAll num xs else x:deleteAll num xs





listsum ::[Int]->Int
listsum []=0
listsum (x:xs)=x+(listsum xs)

sum_x2::[Int]->Int
sum_x2 []=0
sum_x2 (x:xs) = x^2+ sum_x2 xs

deviation::[Int]->Double
deviation [] =0
deviation (x:xs)=realToFrac(res)
    where
        len=length (x:xs)
        preres= fromIntegral(sum_x2 (x:xs))/fromIntegral(len) - ((fromIntegral(listsum (x:xs)))/fromIntegral(len))**2
        res=preres**0.5


lastSegment::Eq a=> [a]->[a]->Bool
lastSegment [] []=True  
lastSegment [] (y:ys)=False
lastSegment (x:xs) []=False
lastSegment (x:xs) (y:ys)= revxs==fragrevys 
    where
        revxs= reverse (x:xs)
        revys= reverse (y:ys)
        xslen = length (x:xs)
        fragrevys= take xslen revys
        
       
seiretsu ::[Int] -> [Int]
seiretsu [] = []
seiretsu (x:xs) = seiretsu l ++ [x] ++ seiretsu r
  where
    l = filter (<x) xs
    r = filter (>=x) xs


--https://qiita.com/nobsun/items/babd28fe81ba3b9f304f
narabi :: [a] -> [[a]]
narabi []     = [[]]
narabi (x:xs) = concatMap ((\x xs -> map ((\x (ys,zs)->ys++x:zs) x) (newdevide xs)) x) (narabi xs)


newdevidesub :: [a]->Int->([a],[a])
newdevidesub [] 0=([],[])
newdevidesub (x:xs) 0=([],(x:xs))
newdevidesub (x:xs) index=case ((0<=index)&&(index<=(length (x:xs))))of
  True->((take index (x:xs)),(drop index (x:xs)))
  False->([],[]) -- error

newdevidesub2 ::a->([a],[a])->([a],[a]) -- newdevidesub2 ([1,2],[3,4]) 5   -> ([5,1,2],[3,4])
--newdevidesub2 a_pair []=a_pair
newdevidesub2 elm a_pair =(newpair_fst,(snd a_pair))
    where
      a_pair_fst=fst a_pair
      newpair_fst=concat[[elm],a_pair_fst]

newdevidesub2map ::a->[([a],[a])]->[([a],[a])]
newdevidesub2map elm []=[]
newdevidesub2map elm (x:xs)=
  [(newdevidesub2 elm x)]++(newdevidesub2map elm xs)

newdevide::[a]->[([a],[a])]
newdevide []=[([],[])]
newdevide (x:xs)=[(newdevidesub (x:xs) 0)]++(newdevidesub2map x (newdevide xs))


sentakusub :: a->[[a]]->[[a]] -- (sentakusub 1 [[1,2,3],[4,5,6],[7,8,9]])->[[1,1,2,3],[1,4,5,6],[1,7,8,9]]
sentakusub elm []=[]
sentakusub elm (x:xs)= concat[[([elm]++x)],(sentakusub elm xs)]
sentaku ::[a]->Int->[[a]]
sentaku (x:xs) 0=[[]]
sentaku (x:[]) 1 =[[x]]
sentaku (x:xs) n=case (length (x:xs))==n of
  True-> [(x:xs)]
  False ->(sentaku xs n)++(sentakusub x (sentaku xs (n-1))) 


