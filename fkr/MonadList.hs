module MonadList (returnList, bindList) where

returnList :: a -> [a]
returnList x = [x]

bindList :: [a] -> (a -> [b]) -> [b]
bindList xs f = concatMap f xs
