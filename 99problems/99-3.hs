-- 99 problems, #3
-- find the k'th element of a list

popKth :: (Integral b) => [a] -> b -> a
popKth ([]) _ = error "the list isn't long enough!"
popKth (x:xs) k
  | k < 0 = error "how do you find a negativeth item?!?!"
  | k == 0 = x
  | k > 0 = popKth xs (k-1)
--popKth (x:xs) 0 = x
--popKth (x:xs) k = popKth xs (k-1)