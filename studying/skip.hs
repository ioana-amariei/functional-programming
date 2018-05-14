skipWhiteSpace :: String -> String
skipWhiteSpace (' ':xs) = skipWhiteSpace xs
skipWhiteSpace s = s
