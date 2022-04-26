--Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail.
--As a hint, you might want to consider using the following types.safeHead :: [a] -> Maybe a

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit xs     = Just (go xs)
  where
    go [_]    = []
    go (x:xs) = x : go xs