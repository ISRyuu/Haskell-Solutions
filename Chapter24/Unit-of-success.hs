import Text.Trifecta

main :: IO ()
main = do
  print $ parseString parser mempty "123"
    where parser = integer >>= (\c -> eof >> return c)
