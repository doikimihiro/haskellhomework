data Status=Status{t::Int,w::Int,g::Int,c::Int}
node=replicate 16 defaut_status
    where
        defaut_status=Status{t=0,w=0,g=0,c=0}
--16頂点のグラフを構築し辺をすべてはった上で全探索する
inhibit ::Int ->Bool　
inhibit step=((t (node!!step))==(g(node!!step)))&&(((t (node!!step))==(w(node!!step)))||((t (node!!step))==(c(node!!step))))


previous ::Int->Int->Bool
previous step 0=((t (node!!0))==nowt)&&((w (node!!0))==noww)&&((g (node!!0))==nowg)&&((c (node!!0))==nowc)
    where
        nowt=t (node!!step)
        noww=w (node!!step)
        nowg=g (node!!step)
        nowc=c (node!!step)
previous step s=(previous step (s-1))&&((t (node!!step))==nowt)&&((w (node!!step))==noww)&&((g (node!!step))==nowg)&&((c (node!!step))==nowc)
    where
        nowt=t (node!!step)
        noww=w (node!!step)
        nowg=g (node!!step)
        nowc=c (node!!step)

previous_step ::Int->Bool
previous_step step=previous step step

final_step ::Int -> Bool --Trueなら全員が向こう岸にたどり着いた
final_step step=((t (node!!step))==1)&&((w (node!!step))==1)&&((g (node!!step))==1)&&((c (node!!step))==1)

only_t ::Int->()
only_t step= 
    where 
        next=step+1

search ::Int->()
search step=case final_step step of
    True-> ()
    False->case inhibit step||previous_step step of
        True->()
        False -> 
    where
        next=step +1
        nowt=t(node!!step)