module API

data API : Type -> Type -> Type where
  Api :  {input : Type} -> {output : Type} -> API input output

mutual
  data A : Type where
    A1 : String -> A
    A2 : Int -> A
  data IsA1 : A -> Type where
    IA1 : forall s. IsA1 (A1 s)


def : IsA1 (A1 s)
def = IA1 

Name : Type
Name = String

data Id : Type -> Type where
  Id' : rep -> Id rep
mutual
  UserId : Type
  UserId = Id String
  data User : Type where
    U1 : Id String -> Name -> User

userAPI : API UserId User





{-
{-data B = B1 String | B2 String Int String
-}

g : String -> String
g x = x
f : String -> String
f x = g x

h : String -> Int -> String
h = \x, y => g x

j : String -> Int -> String
j x y = x
-}

{-
mutual 
  data Idu : Type where
    MIdu : String -> Idu
    SIdu : String -> (m : Idu) -> IsMIdu m -> Idu
  data IsMIdu : Idu -> Type where
    mIdu : IsMIdu (MIdu n)

Name : Type
Name = String
Date : Type
Date = String
UserId : Type
UserId = String

record User  where
  constructor MkUser
  userId : UserId
  name : Name
  birthDate : Date

userAPI : UserId -> User 
userAPI x = ?U
  
Name : Type
Name = String

UserId : Type
UserId = String

BirthDate : Type
BirthDate = String

data User = User' UserId Name BirthDate

data API : Type where
  api : (i : Type) -> (o : Type) -> API
  
userAPI : API
userAPI = api UserId User

apiList : List API
apiList = [userAPI]
-}
