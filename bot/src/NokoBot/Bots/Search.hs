module NokoBot.Bots.Search 
  ( Search
  ) where

import RIO
import NokoBot.Types

data Search = Search

instance Bot (Search) where
  type DepF Search = HasLogFunc
  name _ = "Search"
  init = return Search 
  reply _ _ = do
    return $ Just "serhoge"

