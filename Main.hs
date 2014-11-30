module Main where

import ChristmasTree (runAnimation)
import Control.Concurrent.Suspend.Lifted (msDelay)

main :: IO ()
main = runAnimation $ msDelay 700
