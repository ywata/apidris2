module Test.API

import Common

UserId : Type
record User where
  userId : UserId
  name : String

userAPI : API UserId (Int, Double, String)

