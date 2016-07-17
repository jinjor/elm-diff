import Html exposing (..)

import Diff

main =
  div []
  [ pre [] [ text a ]
  , hr [] []
  , pre [] [ text b ]
  , hr [] []
  , result (Diff.diffLines a b)
  ]


result changes =
  ul [] (List.map each changes)


each change =
  li [] [ text (toString change) ]


a = """aaa
bbb
ddd"""

b = """zzz
aaa
ccc
ddd"""
