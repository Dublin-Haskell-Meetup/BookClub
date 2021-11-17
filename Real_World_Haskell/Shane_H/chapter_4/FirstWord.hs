firstWord s = unlines (map (safeHead . words) (lines s))
  where
    safeHead ws =
      if null ws
        then ""
        else head ws