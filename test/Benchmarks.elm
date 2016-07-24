import ElmTest exposing (..)
import Html exposing (text)
import Diff exposing (..)
import String
import List


runManyTimes : Int -> String -> String -> Assertion
runManyTimes times a b =
  let
    total =
      List.foldl (\i n -> n + List.length (diffLines a b)) 0 [1..times]
  in
    assert (total > 0)


tests : Test
tests =
  suite "A Test Suite"
    [ test "exactly same" (runManyTimes 100 a a)
    , test "add line to first" (runManyTimes 100 a b)
    , test "add line to last" (runManyTimes 100 a c)
    , test "drop first line" (runManyTimes 100 a d)
    , test "remove line at middle" (runManyTimes 100 a e)
    , test "add line at middle" (runManyTimes 100 a f)

    -- O(ND): 0.63s ( O(ND) = (280*2)*(280*2) )
    -- O(NP): 0.32s ( O(NP) = (280*2)*((280*2-0)/2) )
    , test "modify all" (runManyTimes 10 a g)

    -- O(ND): 0.13s ( O(ND) = 280*280 )
    -- O(NP): 0.0s ( O(NP) = 280*((280-280)/2) )
    , test "add all" (runManyTimes 10 "" a)

    -- O(ND): 0.13s ( O(ND) = 280*280 )
    -- O(NP): 0.0s ( O(NP) = 280*((280-280)/2) )
    , test "remove all" (runManyTimes 10 a "")
    ]


main : Program Never
main =
  runSuite tests


b = "first\n" ++ a
c = a ++ "\nlast"
d = mapLines (List.drop 1) a
e = mapLines (List.take 100) a ++ mapLines (List.drop 101) a
f = mapLines (List.take 101) a ++ mapLines (List.drop 100) a
g = mapEachLine ((++) "_") a

mapLines f s =
  String.join "\n" (f (String.lines s))

mapEachLine f s =
  mapLines (List.map f) s

a = """
{ a =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, b =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, c =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, d = 0
, e =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, f =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, g =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, h =
    { a = 1
    , b = 1
    , c = 1
    , d = 1
    , e = "1"
    , f = "1"
    , g = "1"
    , h = "1"
    , i = "1"
    , j = "1"
    , k = "1"
    }
, i = 0
, j =
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
, k =
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
, l =
    "ccccccccccccccccccccccccccccccccccc"
, o =
    "dddddddddddddddddddddddddddddddddddddddddddddddddddddd"
, p =
    Just
      ( Just
          ( Just
              ( Just
                  ( Just
                      ( Just ( Just ( Just ( Just 1 ) ) ) )
                  )
              )
          )
      )
, q =
    Just
      ( Just
          ( Just
              ( Just
                  ( Just
                      ( Just ( Just ( Just ( Just 2 ) ) ) )
                  )
              )
          )
      )
, r =
    Just
      ( Just
          ( Just
              ( Just
                  ( Just
                      ( Just ( Just ( Just ( Just 3 ) ) ) )
                  )
              )
          )
      )
, s =
    Just
      ( Just
          ( Just
              ( Just
                  ( Just
                      ( Just ( Just ( Just ( Just 4 ) ) ) )
                  )
              )
          )
      )
, t = "Ok, Google"
, u = 123456789
, v = 123.456
, w =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, x =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, y =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, z =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
}
"""
