module Lib1
  ( completions,
  )
where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions =
  [ "player_name",
    "player_damage",
    "enemy_name",
    "enemy_damage",
    "enemy_stats",
    "enemy_type",
    "health_value"
  ]
