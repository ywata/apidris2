module Common

public export
data API : Type -> Type -> Type where
  Api :  {input : Type} -> {output : Type} -> API input output

public export
Id : Type -> Type -> Type
Id s t = t
