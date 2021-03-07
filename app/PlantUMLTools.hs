module PlantUMLTools where

import Data.Text as T

import Language.PlantUML



isArrowDef :: Declaration -> Bool
isArrowDef (ArrowDef _) = True
isArrowDef _ = False

eqName :: T.Text -> Name -> Bool
eqName n (Q name) = n == name
eqName n (Nq name) = n == name


data HorizontalDirection = L | R | LR
  deriving(Show, Eq, Bounded, Enum)


hasLeftName, hasRightName, hasSourceName, hasTargetName :: Arrow -> T.Text -> Bool
hasLeftName (Arrow (Just name) _ _ _ _) n = eqName n name
hasLeftName (ActivationArrow (Just name) _ _ _ _) n = eqName n name
hasLeftName _ _ = False

hasRightName (Arrow _ _ (Just (Name1 name)) _ _) n = eqName n name
hasRightName (Arrow _ _ (Just (AliasedName name1 name2 )) _ _) n = eqName n name1 || eqName n name2
hasRightName (Arrow _ _ (Just (NameColor name _)) _ _) n = eqName n name
hasRightName (ActivationArrow _ _ name _ _) n = eqName n name
hasRightName (Return _) n = False
hasRightName _ _ = False

hasSourceName = undefined
hasTargetName = undefined

getLeftName, getRightName, getSourceName, getTargetName :: Arrow -> Maybe Name
getLeftName (Arrow (Just name) _ _ _ _) = Just name
getLeftName (ActivationArrow name _ _ _ _) = name
getLeftName  _ = Nothing

getRightName (Arrow _ _ (Just (Name1 name)) _ _) = Just name
getRightName (Arrow _ _ (Just (AliasedName n@(Q name1) (Q name2 ))) _ _) = Just n -- this should not happen?
getRightName (Arrow _ _ (Just (AliasedName (Q name1) n@(Nq name2 ))) _ _) = Just n
getRightName (Arrow _ _ (Just (AliasedName n@(Nq name1) (Q name2 ))) _ _) = Just n
getRightName (Arrow _ _ (Just (AliasedName n@(Nq name1) (Nq name2 ))) _ _) = Just n -- this should not happen?
getRightName (Arrow _ _ (Just (NameColor name _)) _ _) = Just name
getRightName (ActivationArrow _ _ n _ _) = Just n
getRightName _ = Nothing

getSourceName = undefined
getTargetName = undefined


getMessage :: Declaration -> Maybe T.Text
getMessage (ArrowDef (Arrow _ _ _ _ msg)) = msg
getMessage (ArrowDef (ActivationArrow _ _ _ _ msg)) = msg
getMessage (ArrowDef (Return msg)) = msg
getMessage _ = Nothing

