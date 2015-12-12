module Helper where

import Data.Time

unpackMilliseconds :: Milliseconds -> Number
unpackMilliseconds (Milliseconds n) = n
