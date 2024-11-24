module Lib1
  ( completions,
  )
where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions =
  [ -- commands
    "init_game",
    "spawn_player",
    "spawn_enemy",
    "remove_player",
    -- info
    "player",
    "enemy",
    "entity_name",
    "health_value",
    "entity_damage",
    "enemy_type",
    -- types
    "Rusher",
    "Bruiser",
    "Ranger"
  ]
