diff :: (Double -> Double) -> Double -> (Double -> Double)
diff f dx = \x -> (f (x + dx) - f x) / dx

newton_iter :: (Double -> Double) -> (Double -> Double) -> Double -> Int -> Double
newton_iter f f' x 0 = x
newton_iter f f' x k = newton_iter f f' (x - f x / f' x) (k - 1)

newton_with_diff :: (Double -> Double) -> Double -> Double -> Int -> Double
newton_with_diff f dx x0 k = newton_iter f (diff f dx) x0 k

main :: IO ()
main = do
    let dx = 0.01
        k = 10

    let f1 x = sin x
    let x1 = newton_with_diff f1 dx 0.5 k
    print $ "Нуль функції f(x) = sin(x), знайдений методом Ньютона: " ++ show x1

    let f2 x = x^3 - 328 * x^2 - 1999 * x - 1670
    let x2 = newton_with_diff f2 dx 100 k
    print $ "Нуль функції f(x) = x^3 - 328x^2 - 1999x - 1670, знайдений методом Ньютона: " ++ show x2
