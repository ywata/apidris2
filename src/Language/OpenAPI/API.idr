
Name : Type
Name = String
BirthDay : Type
BirthDay = String
data UserId = UserId' String
data User = User UserId Name BirthDay

URL : Type
URL = String

userAPI : Type
userAPI = UserId -> User



