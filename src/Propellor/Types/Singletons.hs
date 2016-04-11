{-# LANGUAGE CPP, DataKinds, PolyKinds, TypeOperators, TypeFamilies, GADTs, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

-- | Basic implementation of singletons, portable back to ghc 7.6.3

module Propellor.Types.Singletons (
	module Propellor.Types.Singletons,
	KProxy(..),
	Proxy(..),
	Nat,
) where

#if __GLASGOW_HASKELL__ > 707
import GHC.TypeLits
import Data.Proxy (KProxy(..), Proxy(..))
#else
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
data KProxy (a :: *) = KProxy
#endif

-- | The data family of singleton types.
data family Sing (x :: k)

-- | A class used to pass singleton values implicitly.
class SingI t where
	-- | Generate a singleton.
	sing :: Sing t

class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
	type DemoteRep kparam :: *
	-- | From singleton to value.
	fromSing :: Sing (a :: k) -> DemoteRep kparam

-- Lists of singletons
data instance Sing (x :: [k]) where
	Nil :: Sing '[]
	Cons :: Sing x -> Sing xs -> Sing (x ': xs)
instance (SingI x, SingI xs) => SingI (x ': xs) where sing = Cons sing sing
instance SingI '[] where sing = Nil
instance SingKind ('KProxy :: KProxy a) => SingKind ('KProxy :: KProxy [a]) where
	type DemoteRep ('KProxy :: KProxy [a]) = [DemoteRep ('KProxy :: KProxy a)]
	fromSing Nil = []
	fromSing (Cons x xs) = fromSing x : fromSing xs

-- Singleton bools
data instance Sing (x :: Bool) where
	TrueS :: Sing 'True
	FalseS :: Sing 'False
instance SingI 'True where sing = TrueS
instance SingI 'False where sing = FalseS
instance SingKind ('KProxy :: KProxy Bool) where
	type DemoteRep ('KProxy :: KProxy Bool) = Bool
	fromSing FalseS = False
	fromSing TrueS = True

-- Singleton nats
type SNat (x :: Nat) = Sing x
#if __GLASGOW_HASKELL__ > 707
data instance Sing (n :: Nat) = KnownNat n => SNat
instance KnownNat n => SingI n where sing = SNat
instance SingKind ('KProxy :: KProxy Nat) where
	type DemoteRep ('KProxy :: KProxy Nat) = Integer
	fromSing (SNat :: Sing n) = natVal (Proxy :: Proxy n)
#else
data instance Sing (n :: Nat) = TL.SingRep n Integer => SNat
instance TL.SingRep n Integer => SingI (n :: Nat) where sing = SNat
instance SingKind ('KProxy :: KProxy Nat) where
	type DemoteRep ('KProxy :: KProxy Nat) = Integer
	fromSing (SNat :: Sing n) = TL.fromSing (TL.sing :: TL.Sing n)
#endif
