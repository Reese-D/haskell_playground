module Main (main) where

import SimpleJSON

main = print (JObject [("new JObject", JNumber 10), ("truthy value", JBool False)])
