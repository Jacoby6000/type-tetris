module Data() where


data Question a = Question { problem :: TypeSignature a, solution :: Binding a }

data TypeSignature a
  = TTVar (TVar a)
  | Forall (TVar a) TypeSignature
  | Function TypeSignature TypeSignature


data Binding a
  = Var a
  | App (Binding a) (Binding a)
  | Lam a (Binding a)


data TVar a = TVar a (Maybe a)


class PrettyPrint a where
   prettyPrint :: a -> String

instance PrettyPrint a => PrettyPrint (TypeSignature a) where
   prettyPrint (TTVar (TVar a b)) = \x -> "(" ++ prettyPrint a ++ " " ++ x ++ ")" <$> prettyPrint <$> b prettyPrint a
   prettyPrint (Forall (TVar a) ts) = "forall " ++ prettyPrint a ++ ". " ++ prettyPrint ts
   prettyPrint (Function l r) =
     case l of (Function _ _) -> "(" ++ prettyPrint l ++ ") -> " ++ prettyPrint r
               (Forall _ _) -> "(" ++ prettyPrint l ++ ") -> " ++ prettyPrint r
               (TTVar _) -> prettyPrint l ++ " -> " ++ prettyPrint r

