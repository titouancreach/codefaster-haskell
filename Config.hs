module Config where 

  import Text.Regex

  timeout :: Int
  timeout = 10

  ignore :: [Regex]
  ignore = map mkRegex [
      "^-- " -- Ignore haskell comments
    ]
