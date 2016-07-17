module Diff exposing (Change(..), diff, diffLines)

import Array exposing (Array)
import String


type Change a
  = Add a
  | NoChange a
  | Remove a


diffLines : String -> String -> List (Change String)
diffLines a b =
  diff (String.lines a) (String.lines b)


diff : List a -> List a -> List (Change a)
diff a b =
  let
    arrA =
      Array.fromList a

    arrB =
      Array.fromList b

    m =
      Array.length arrA

    n =
      Array.length arrB

    getA =
      (\x -> Array.get (x - 1) arrA)

    getB =
      (\y -> Array.get (y - 1) arrB)

    getAOrCrash x =
      case getA x of
        Just a -> a
        Nothing -> Debug.crash ("Cannot get A[" ++ toString x ++ "]")

    getBOrCrash y =
      case getB y of
        Just b -> b
        Nothing -> Debug.crash ("Cannot get B[" ++ toString y ++ "]")

    path =
      ond getA getB m n

  in
    List.reverse (makeChanges getAOrCrash getBOrCrash path)


makeChanges : (Int -> a) -> (Int -> a) -> List (Int, Int) -> List (Change a)
makeChanges getA getB path =
  case path of
    [] ->
      []
    latest :: tail ->
      makeChangesHelp getA getB latest tail


makeChangesHelp : (Int -> a) -> (Int -> a) -> (Int, Int) -> List (Int, Int) -> List (Change a)
makeChangesHelp getA getB (x, y) path =
  case path of
    [] ->
      []
    (prevX, prevY) :: tail ->
      let
        change =
          if x - 1 == prevX && y - 1 == prevY then
            NoChange (getA x)
          else if x == prevX then
            Add (getB y)
          else if y == prevY then
            Remove (getA x)
          else
            Debug.crash ("Unexpected path: " ++ toString ((x, y), path))
      in
        change :: makeChangesHelp getA getB (prevX, prevY) tail


ond : (Int -> Maybe a) -> (Int -> Maybe a) -> Int -> Int -> List (Int, Int)
ond getA getB m n =
  let
    v =
      Array.initialize (m + n + 1) (always [])
  in
    ondHelp (snake getA getB) m 0 0 v


ondHelp : (Int -> Int -> List (Int, Int) -> (List (Int, Int),Bool)) -> Int -> Int -> Int -> Array (List (Int, Int)) -> List (Int, Int)
ondHelp snake offset d k v =
  if d >= Array.length v then
    Debug.crash ("Unexpected index out of bounds: " ++ toString d)
  else if k > d then
    ondHelp snake offset (d + 1) (-d - 1) v
  else
    let
      fromLeft =
        Maybe.withDefault [] (Array.get (k - 1 + offset) v)

      fromTop =
        Maybe.withDefault [] (Array.get (k + 1 + offset) v)

      (path, (x, y)) =
        case (fromLeft, fromTop) of
          ([], []) ->
            ([], (0, 0))
          ([], (topX, topY) :: _) ->
            (fromTop, (topX + 1, topY))
          ((leftX, leftY) :: _, []) ->
            (fromLeft, (leftX, leftY + 1))
          ((leftX, leftY) :: _, (topX, topY) :: _) ->
            -- this implies "remove" comes always earlier than "add"
            if leftY + 1 >= topY then
              (fromLeft, (leftX, leftY + 1))
            else
              (fromTop, (topX + 1, topY))

      (newPath, goal) =
        snake (x + 1) (y + 1) ((x, y) :: path)
    in
      if goal then
        newPath
      else
        let
          newV =
            Array.set (k + offset) newPath v
        in
          ondHelp snake offset d (k + 2) newV


snake : (Int -> Maybe a) -> (Int -> Maybe a) -> Int -> Int -> List (Int, Int) -> (List (Int, Int), Bool)
snake getA getB nextX nextY path =
  case (getA nextX, getB nextY) of
    (Just a, Just b) ->
      if a == b then
        snake getA getB (nextX + 1) (nextY + 1) ((nextX, nextY) :: path)
      else
        (path, False)

    -- reached bottom-right corner
    (Nothing, Nothing) ->
      (path, True)

    _ ->
      (path, False)
