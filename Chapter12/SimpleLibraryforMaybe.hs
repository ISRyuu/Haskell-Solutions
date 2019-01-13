isJust :: Maybe t -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe t -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ Nothing = b

fromMaybe :: t -> Maybe t -> t
fromMaybe t Nothing = t
fromMaybe t (Just v) = v

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe (Just x:xs) =
  case rest of
    Nothing -> Nothing
    Just zs -> Just (x : zs)
  where rest = flipMaybe xs
