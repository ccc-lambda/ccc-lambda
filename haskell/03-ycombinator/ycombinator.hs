-- Y combinator
y :: (a -> a) -> a
y f = (\x -> f (x x)) (\x -> f (x x))

-- 用範例展示如何使用 Y 組合子定義遞歸
factorial :: (Integer -> Integer) -> Integer -> Integer
factorial _ 0 = 1
factorial f n = n * f (n - 1)

-- 使用 Y 組合子求階乘
main :: IO ()
main = print (y factorial 5)  -- 輸出 120
