data Status=Status{t_::Int,w_::Int,g_::Int,c_::Int}
type Path =[Status]

a=Status{t_=0,w_=0,g_=0,c_=0}
b=Status{t_=0,w_=0,g_=0,c_=1}
c=Status{t_=0,w_=0,g_=1,c_=0}
d=Status{t_=0,w_=0,g_=1,c_=1}
e=Status{t_=0,w_=1,g_=0,c_=0}
f=Status{t_=0,w_=1,g_=0,c_=1}
g=Status{t_=0,w_=1,g_=1,c_=0}
h=Status{t_=0,w_=1,g_=1,c_=1}
i=Status{t_=1,w_=0,g_=0,c_=0}
j=Status{t_=1,w_=0,g_=0,c_=1}
k=Status{t_=1,w_=0,g_=1,c_=0}
l=Status{t_=1,w_=0,g_=1,c_=1}
m=Status{t_=1,w_=1,g_=0,c_=0}
n=Status{t_=1,w_=1,g_=0,c_=1}
o=Status{t_=1,w_=1,g_=1,c_=0}
p=Status{t_=1,w_=1,g_=1,c_=1}

allPlaces=a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:[]



--禁止状態でなければTrueを返す
ok_node ::Status->Bool
ok_node vertex =case  (t_ vertex/=g_ vertex)&&((w_ vertex==g_ vertex)||(c_ vertex==g_ vertex)) of
    True -> False
    False ->True

--移動前と移動先の頂点が存在することを確認し、その後その移動が可能かどうかを確認する
transition ::Status->Status->Bool
transition from to =case (ok_node from)&&(ok_node to) of
    False -> False --そもそも、状態a,bのどちらかが禁止状態である
    True -> case (t_ from)/=(t_ to)&&(w_ from)==(w_ to)&&(g_ from)==(g_ to)&&(c_ from)==(c_ to) of --農夫は必ず移動しなければならないので移動しているかチェックする
            True ->True
            False -> case  (t_ from)/=(t_ to)&&(w_ from)/=(w_ to)&&(t_ from)==(w_ from)&&(g_ from)==(g_ to)&&(c_ from)==(c_ to) of
                True->True
                False -> case  (t_ from)/=(t_ to)&&(g_ from)/=(g_ to)&&(t_ from)==(g_ from)&&(c_ from)==(c_ to)&&(w_ from)==(w_ to) of
                    True ->True
                    False -> case  (t_ from)/=(t_ to)&&(c_ from)/=(c_ to)&&(t_ from)==(c_ from)&&(g_ from)==(g_ to)&&(w_ from)==(w_ to) of
                        True ->True
                        False ->False

            


before :: Path->Status->Bool --以前通ったことがある頂点が存在すればFalseを返す
before [] vertex =True
before path vertex = case (t_ (head path))==(t_ vertex)&&(g_ (head path))==(g_ vertex)&&(w_ (head path))==(w_ vertex)&&(c_ (head path))==(c_ vertex)of
    True -> False
    False ->before(tail path) vertex

-- ここから深さ優先探索
search ::Status->Status->Path
search current_point goal_point = depth current_point goal_point []

depth ::Status->Status->Path->Path --この関数がよばれる旅にpathはcpが追加され先に伸びていく
depth cp gp path|(t_ cp)==(t_ gp)&&(w_ cp)==(w_ gp)&&(g_ cp)==(g_ gp)&&(c_ cp)==(c_ gp) =gp:path --つまり cp==gp
                |otherwise =breadth(reachable cp path) gp (cp:path)

breadth ::[Status]->Status ->Path->Path
breadth [] gp path =[]
breadth (cp:cps) gp path|null ans =breadth cps gp path
                        |otherwise=ans
                where
                  ans =depth cp gp path

reachable ::Status->Path->[Status]
reachable cp path =reachable' cp path allPlaces

reachable'::Status->Path->[Status]->[Status]
reachable' cp path []=[]
reachable' cp path (np:nps)=case (transition cp np)of
                            True->case (before path np) of
                                True->np:(reachable' cp path nps)
                                False->reachable' cp path nps
                            False ->reachable' cp path nps

-- reachable' a [] allPlaces =i:n:o:[]

convertstring ::Status->String
convertstring status   =case (t_ status==0) &&(w_ status==0)&&(g_ status== 0)&&(c_ status ==0)  of
    True->"人狼羊草|"
    False ->case (t_ status==0) &&(w_ status==0)&&(g_ status== 0)&&(c_ status ==1)of
        True->"人狼羊|草"
        False -> case (t_ status==0) &&(w_ status==0)&&(g_ status== 1)&&(c_ status ==0)of
                    True->"人狼草|羊"
                    False->case (t_ status==0) &&(w_ status==0)&&(g_ status== 1)&&(c_ status ==1)of
                        True->"人狼|羊草"
                        False->case(t_ status==0) &&(w_ status==1)&&(g_ status== 0)&&(c_ status ==0)of
                                    True->"人狼草|狼"
                                    False ->case(t_ status==0) &&(w_ status==1)&&(g_ status== 0)&&(c_ status ==1)of
                                                True->"人羊|狼草"
                                                False->case(t_ status==0) &&(w_ status==1)&&(g_ status== 1)&&(c_ status ==0)of
                                                    True -> "人草|狼羊"
                                                    False -> case(t_ status==0) &&(w_ status==1)&&(g_ status== 1)&&(c_ status ==1)of
                                                        True->"人|狼羊草"
                                                        False->case(t_ status==1) &&(w_ status==0)&&(g_ status== 0)&&(c_ status ==0)of
                                                            True->"狼羊草|人"
                                                            False->case(t_ status==1) &&(w_ status==0)&&(g_ status== 0)&&(c_ status ==1)of
                                                                True->"狼羊|草人"
                                                                False ->case(t_ status==1) &&(w_ status==0)&&(g_ status== 1)&&(c_ status ==0)of
                                                                        True ->"狼草|人羊"
                                                                        False ->case(t_ status==1) &&(w_ status==0)&&(g_ status== 1)&&(c_ status ==1)of
                                                                            True->"狼|人羊草"
                                                                            False ->case(t_ status==1) &&(w_ status==1)&&(g_ status== 0)&&(c_ status ==0)of
                                                                                True->"羊|人狼草"
                                                                                False ->case(t_ status==1) &&(w_ status==1)&&(g_ status== 0)&&(c_ status ==1)of 
                                                                                    True->"羊|人狼草"
                                                                                    False ->case(t_ status==1) &&(w_ status==1)&&(g_ status== 1)&&(c_ status ==0)of
                                                                                        True ->"草|人狼羊"
                                                                                        False->case(t_ status==1) &&(w_ status==1)&&(g_ status== 1)&&(c_ status ==1)of
                                                                                            True->"|人狼羊草"
                                                                                            False->"エラー"



output ::Path->IO()
output []=putStrLn $ ""
output path =(putStrLn $ (convertstring (head path)) ++ "\n") >>(output (tail path)) 


result ::() -> IO()
result ()=output (reverse (search a p))



                            