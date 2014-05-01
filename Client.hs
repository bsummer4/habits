{-# LANGUAGE UnicodeSyntax #-}

import Data.Time.Calendar() -- Show instance for Day.
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Functor

main = do
	today ← utctDay <$> getCurrentTime
	print today
