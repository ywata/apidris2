module API

f : a -> b -> c -> d -> d
f a b c d = d

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

  record Date  where
    year : Int
    month : Int
    day : Int
      
  data User : Type where
    U1 : Id String -> Name -> Date -> User

  UserId : Type
  UserId = Id {ty = User} String


record DateTime where
  date : Date
  time : Int

userAPI : API (UserId -> UserId) User
usersAPI : API () (List User)

listUsersAPI : API (List UserId) (List User -> User)
listUsersAPI' : API (String) (List User)

listUserbyNameAPI : API (Name) (List User)
updateUserAPI : API (User) ()
primAPI : API String User

namespace API.Test
  userAPI : API (UserId -> UserId) User
  usersAPI : API () (List User)
  
