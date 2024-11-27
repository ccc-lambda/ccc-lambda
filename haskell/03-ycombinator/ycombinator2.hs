-- 使用 newtype 創建 Y 組合子
newtype Fix f = Fix (f (Fix f))

y :: (a -> a) -> a
y f = (\x -> f (x x)) (\x -> f (x x))

-- 階乘範例
factorial :: (Integer -> Integer) -> Integer -> Integer
factorial _ 0 = 1
factorial f n = n * f (n - 1)

main :: IO ()
main = print (y factorial 5)
