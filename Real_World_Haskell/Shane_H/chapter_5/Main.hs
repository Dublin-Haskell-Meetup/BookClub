module Main (main) where -- need to pass main to Main, errata from textbook

import SimpleJSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])