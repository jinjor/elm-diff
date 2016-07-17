import ElmTest exposing (..)
import Html exposing (text)
import Diff exposing (..)

tests : Test
tests =
  suite "A Test Suite"
    [ test "basic" (assertEqual [] (diff [] []) )
    , test "basic" (assertEqual [Removed 1] (diff [1] []) )
    , test "basic" (assertEqual [Added 1] (diff [] [1]) )
    , test "basic" (assertEqual [NoChange 1] (diff [1] [1]) )
    , test "basic" (assertEqual [NoChange 1, Removed 2] (diff [1,2] [1]) )
    , test "basic" (assertEqual [Removed 1, NoChange 2] (diff [1,2] [2]) )
    , test "basic" (assertEqual [NoChange 1, Added 2] (diff [1] [1,2]) )
    , test "basic" (assertEqual [Added 1, NoChange 2] (diff [2] [1,2]) )
    , test "basic" (assertEqual [NoChange 1, NoChange 2] (diff [1,2] [1,2]) )
    , test "basic" (assertEqual [Removed 1, Removed 2] (diff [1,2] []) )
    , test "basic" (assertEqual [Added 1, Added 2] (diff [] [1,2]) )
    , test "basic" (assertEqual [Removed 1, Added 2] (diff [1] [2]) )
    , test "basic" (assertEqual [Removed 1, Added 2, NoChange 3] (diff [1, 3] [2, 3]) )
    ]

main : Program Never
main =
  runSuite tests

c = """
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
