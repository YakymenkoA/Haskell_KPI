type IntSet = Int -> Bool

isMember :: IntSet -> Int -> Bool
isMember f x = f x

emptySet :: IntSet
emptySet _ = False

allInts :: IntSet
allInts _ = True

interval :: Int -> Int -> IntSet
interval lBound uBound x = x >= lBound && x <= uBound

coprimeSet :: Int -> IntSet
coprimeSet k x = gcd k x == 1

setIntersection :: IntSet -> IntSet -> IntSet
setIntersection s1 s2 x = s1 x && s2 x

setUnion :: IntSet -> IntSet -> IntSet
setUnion s1 s2 x = s1 x || s2 x

setComplement :: IntSet -> IntSet -> IntSet
setComplement s1 s2 x = s1 x && not (s2 x)

addToSet :: Int -> IntSet -> IntSet
addToSet y s x = x == y || s x

deleteFromSet :: Int -> IntSet -> IntSet
deleteFromSet y s x = x /= y && s x

areEqual :: IntSet -> IntSet -> Bool
areEqual s1 s2 = all (\x -> isMember s1 x == isMember s2 x) [-1000..1000]

main :: IO ()
main = do
    -- порожня множина
    print $ "isMember emptySet 5: " ++ show (isMember emptySet 5)

    -- множина всіх цілих
    print $ "isMember allInts 5: " ++ show (isMember allInts 5)

    -- інтервал
    let s1 = interval 10 20
    print $ "isMember (interval 10 20) 15: " ++ show (isMember s1 15)
    print $ "isMember (interval 10 20) 25: " ++ show (isMember s1 25)

    -- множина взаємно простих чисел
    let s2 = coprimeSet 6
    print $ "isMember (coprimeSet 6) 5: " ++ show (isMember s2 5)
    print $ "isMember (coprimeSet 6) 4: " ++ show (isMember s2 4)

    -- операції на множинах
    let s3 = interval 1 10
    let s4 = interval 5 15
    print $ "isMember (setIntersection s3 s4) 7: " ++ show (isMember (setIntersection s3 s4) 7)
    print $ "isMember (setUnion s3 s4) 12: " ++ show (isMember (setUnion s3 s4) 12)
    print $ "isMember (setComplement s3 s4) 3: " ++ show (isMember (setComplement s3 s4) 3)

    -- додавання та видалення елементів
    let s5 = addToSet 11 s3
    print $ "isMember (addToSet 11 s3) 11: " ++ show (isMember s5 11)

    let s6 = deleteFromSet 5 s3
    print $ "isMember (deleteFromSet 5 s3) 5: " ++ show (isMember s6 5)

    -- рівність множин
    print $ "areEqual (interval 1 10) (interval 1 10): " ++ show (areEqual (interval 1 10) (interval 1 10))
    print $ "areEqual (interval 1 10) (interval 5 15): " ++ show (areEqual (interval 1 10) (interval 5 15))
