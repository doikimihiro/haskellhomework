data Status=Status{t::Int,w::Int,g::Int,c::Int}
node=replicate 16 defaut_status
    where
        defaut_status=Status