{-# language UndecidableInstances #-}

module Language.Syntactic.Region.Infer where

import Language.Syntactic
import Language.Syntactic.Functional (Literal, Binding)
import Language.Syntactic.Functional.Tuple (Tuple)

import qualified Language.Syntactic.Functional as S
import qualified Language.Syntactic.Functional.Tuple as S

--------------------------------------------------------------------------------
-- * Regions and their inference.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

class Infer rgn sym
  where
    infer :: sym sig -> Args (AST (sym :&: rgn)) sig -> rgn (DenResult sig)

inferSym :: forall rgn sym sig
   . Infer rgn sym
  => sym sig
  -> Args (AST sym) sig
  -> ASTF (sym :&: rgn) (DenResult sig)
inferSym sym args = go sym $ mapArgs inferASTF args
  where
    go :: sym sig
       -> Args (AST (sym :&: rgn)) sig
       -> ASTF (sym :&: rgn) (DenResult sig)
    go sym args = appArgs (Sym $ sym :&: infer sym args) args

inferASTF :: forall rgn sym sig
   . Infer rgn sym
  => ASTF sym sig
  -> ASTF (sym :&: rgn) sig
inferASTF = match inferSym

--------------------------------------------------------------------------------
-- ** Functional inference.

data Unit a = Unit

instance (Unit :<: rgn) => Infer rgn Literal
  where
    infer (S.Literal a) (Nil) = inj Unit

data Var a = Var S.Name

data Lam rng a where Lam :: rng b -> Lam rng (a -> b)

instance (Var :<: rgn, Lam rgn :<: rgn) => Infer rgn Binding
  where
    infer (S.Var name) (Nil)      = inj $ Var name
    infer (S.Lam name) (a :* Nil) = inj $ Lam $ getDecor a

--------------------------------------------------------------------------------
-- ** Tuple inference.

data Pair rgn a
  where
    Pair :: rgn a -> rgn b -> Pair rgn (a, b)

instance (Pair rgn :<: rgn) => Infer rgn Tuple
  where
    infer (S.Pair) (a :* b :* Nil) = inj (Pair (getDecor a) (getDecor b))
    infer (S.Fst) (pair :* Nil) | Just (Pair a b) <- prj (getDecor pair) = a
    infer (S.Snd) (pair :* Nil) | Just (Pair a b) <- prj (getDecor pair) = b

--------------------------------------------------------------------------------
