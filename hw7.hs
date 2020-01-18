data Nat = Zero | Succ Nat
plus ::Nat->Nat->Nat
plus Zero m =m
plus (Succ n) m = (Succ(plus n m))
instance Show Nat where
    show Zero = "zero"
    show (Succ Zero) = "one"
    show (Succ (Succ Zero)) = "two"
    show (Succ (Succ (Succ Zero))) = "three"
    show (Succ (Succ (Succ (Succ Zero)))) = "four"
    show (Succ (Succ (Succ (Succ (Succ Zero))))) = "five"
    show (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))) = "six"
    show (Succ (Succ (Succ(Succ (Succ (Succ (Succ Zero))))))) = "seven"
    show (Succ (Succ (Succ(Succ (Succ (Succ (Succ (Succ Zero)))))))) = "eight"
    show (Succ (Succ (Succ(Succ (Succ (Succ (Succ (Succ (Succ  Zero))))))))) = "nine"
    show (Succ (Succ (Succ(Succ (Succ (Succ (Succ (Succ (Succ (Succ  Zero)))))))))) = "ten"

zero = Zero
one = Succ Zero
two = Succ one
three = Succ two
four = Succ three
five = Succ four
six = Succ five
seven = Succ six
eight = Succ seven
nine = Succ eight
ten = Succ nine



times ::  Nat -> Nat -> Nat
times Zero m = Zero
times (Succ n) m = plus (times n m) m


--2<=3 をlessthan 2 3と表現したい
lessthan :: Nat-> Nat-> Bool
lessthan Zero m =True
lessthan n Zero=case n of
    Zero -> True
    _ ->False
lessthan (Succ n) (Succ m) =lessthan n m

minus ::  Nat -> Nat -> Nat
minus n Zero= n
minus (Succ n) (Succ m) = minus n m

divide :: Nat -> Nat ->(Nat,Nat)
divide n m = case lessthan m n of  --n/m m<=n
    True ->((Succ (fst (divide (minus n m)  m))),(snd (divide (minus n m) m)))
    False -> (Zero,n)

expt :: Nat -> Nat -> Nat
expt n Zero =one
expt n (Succ m) =times n (expt n m)


