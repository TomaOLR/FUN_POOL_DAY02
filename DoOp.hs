myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
  | x == a = True
  | otherwise = myElem a xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = Just (div a b)

myLength :: [a] -> Int
myLength (x:y) = myLength y + 1
myLength [] = 0

safeNth :: [a] -> Int -> Maybe a
safeNth a n
  | n > (myLength a)  - 1 = Nothing
  | n < 0 = Nothing
safeNth (x:xs) 0 = Just x
safeNth (x:xs) n = safeNth xs (n - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc x = case x of
               Nothing -> Nothing
               Just x -> Just (x + 1)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup a [] = Nothing
myLookup a (x:y)
  | a == myFst x = Just (mySnd x)
  | otherwise = myLookup a y

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo f (Just a) Nothing = Nothing
maybeDo f Nothing (Just b) = Nothing
maybeDo f (Just a) (Just b) = Just (f a b)

isInt :: [Char] -> Bool
isInt [] = True
isInt (x:y) | x >= '0' && x <= '9' = isInt y
            | otherwise = False

readInt :: [Char] -> Maybe Int
readInt str | isInt str = Just (read str)
            | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do
  str <- getLine
  return (myLength (str))

printAndGetLength :: String -> IO Int
printAndGetLength str = do
  putStrLn str
  return (myLength str)

lineToStr :: Int -> [Char] -> IO String
lineToStr 0 str = return str
lineToStr x str = do
  line <- getLine
  lineToStr (x - 1) (str ++ line)

concatLines :: Int -> IO String
concatLines x = do
  str <- (lineToStr x [])
  return str

getInt :: IO (Maybe Int)
getInt = do
  str <- getLine
  if isInt str
    then
    return (Just (read str))
    else
    return Nothing
