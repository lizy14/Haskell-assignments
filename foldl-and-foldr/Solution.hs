-- foldl-and-foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldr :: (b -> a -> a) -> a -> [b] -> a


myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl f z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs
