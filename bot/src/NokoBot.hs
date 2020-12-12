{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NokoBot 
  ( module NokoBot
  , module NokoBot.Types
  , module Markov
  , module Search
  ) where

import RIO
import NokoBot.Types
import NokoBot.Bots.Markov as Markov
import NokoBot.Bots.Search as Search
import Data.Kind     (Type, Constraint)

type AllBotConstraints
  =   DepF Markov
  :&: DepF Search

initAllBots :: AllBotConstraints e => RIO e [SomeBot AllBotConstraints]
initAllBots = sequence 
  [ SomeBot <$> init @Markov
  , SomeBot <$> init @Search ]

