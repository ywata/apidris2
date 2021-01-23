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

mutual
  data Id : {ty :Type} -> Type -> Type where
    Id' : rep -> Id rep
  mutual
    record Date  where
      year : Int
      month : Int
      day : Int
      
    data User : Type where
      U1 : Id String -> Name -> Date -> User

    UserId : Type
    UserId = Id {ty = User} String


userAPI : API (UserId -> UserId) User
usersAPI : API () (List User)

listUsersAPI : API (List UserId) (List User -> User)
listUsersAPI' : API (String) (List User)

listUserbyNameAPI : API (Name) (List User)
updateUserAPI : API (User) ()
primAPI : API String User



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

