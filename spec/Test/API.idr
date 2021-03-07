module Test.API

import Common

UserId : Type
record User where
  userId : UserId
  name : String
data A = A1 Int | A2 String
data B : Type where
  B1 : Int -> B
  B2 : String -> B
  

ユーザ一覧 : API UserId (Int, Double, String)


