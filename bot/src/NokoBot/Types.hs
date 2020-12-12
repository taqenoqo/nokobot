{-# LANGUAGE QuantifiedConstraints #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE UndecidableSuperClasses #-} 
{-# LANGUAGE TypeOperators #-} 
module NokoBot.Types where

import           RIO
import           RIO.Time
import qualified RIO.List as L
import           Data.Kind     (Type, Constraint)

class Bot b where
  type DepF b :: Type -> Constraint
  name :: b -> String
  init :: DepF b e => RIO e b
  reply :: DepF b e => b -> Message -> RIO e (Maybe Text)

class DepF b e => DepC b e
instance DepF b e => DepC b e

data Message = Message
  { messageTime :: ZonedTime
  , messageContent :: Text }

data SomeBot (c :: Type -> Constraint) where
  SomeBot :: (Bot b, forall x. c x => DepC b x) => b -> SomeBot c

instance Bot (SomeBot c) where
  type DepF (SomeBot c) = c
  name (SomeBot b) = name b
  init = init
  reply (SomeBot b) = reply' b where
    reply' :: (Bot b, DepC b e) => b -> Message -> RIO e (Maybe Text) 
    reply' = reply

instance Bot b => Bot [b] where
  type DepF [b] = DepF b
  name bs = "Bot List (" ++ L.intercalate ", " (map name bs) ++ ")"
  init = init
  reply bs msg = asum <$> for bs (`reply` msg)

class    () => CEmpty x
instance () => CEmpty x

class    (c1 x, c2 x) => (c1 :&: c2) x
instance (c1 x, c2 x) => (c1 :&: c2) x

