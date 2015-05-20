{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Propellor.Resources
	( Resource(..)
	, makeService
	, noServices
	, addService
	, Service
	, ServiceDetails(..)
	, getServices
	, startServices
	) where

import GHC.TypeLits
import Control.Monad
import Data.Type.Equality
import Data.Type.Bool

-- Different types of resources; only one service is allowed to use a
-- given resource at a time, and this is checked at the type level.
data Resource = Port Nat

-- Type level equality of Resources.
type family EqResource (a :: Resource) (b :: Resource) where
	EqResource a a = True
	EqResource a b = False
type instance a == b = EqResource a b

-- This data type exists only to make error messages better. 
--
-- Rather than "Couldn't match type ‘'False’ with ‘'True’"
-- The user will see "Couldn't match type ‘'Propellor.Resources.Conflicting’
-- with ‘'Propellor.Resources.NonConflicting’"
data IsResourceConflict = Conflicting | NonConflicting

-- This also appears in error message, as 
-- "Actual type: Propellor.Resources.Conflict"
type family Conflict (uniquelist :: Bool) :: IsResourceConflict where
	Conflict False = Conflicting
	Conflict True = NonConflicting

-- Check if a type-level list contains only unique values.
type family UniqueList (resources :: [Resource]) :: Bool
type instance UniqueList '[] = True
type instance UniqueList (a ': '[]) = True
type instance UniqueList (a ': b ': rest) = Not (a == b) && UniqueList (a ': rest) && UniqueList (b ': rest)

-- Type-level concat.
type family Concat (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Concat '[] list2 = list2
type instance Concat (a ': rest) list2 = a ': Concat rest list2

data Service (resources :: [Resource]) details = Service details

data ServiceDetails = ServiceDetails {
  serviceName :: String
  , runService :: IO ()
  }

makeService :: String -> IO () -> Service resources ServiceDetails
makeService name details = Service (ServiceDetails name details)

noServices :: Service '[] [ServiceDetails]
noServices = Service []

addService :: (finalResources ~ Concat resources newResources, Conflict (UniqueList finalResources) ~ NonConflicting)
  => Service resources [ServiceDetails]
  -> Service newResources ServiceDetails
  -> Service finalResources [ServiceDetails]
addService (Service serviceList) (Service newService) = 
  Service (newService : serviceList)

getServices :: Service resources [ServiceDetails] -> [ServiceDetails]
getServices (Service details) = details

startServices :: [ServiceDetails] -> IO ()
startServices services = forM_ services $ \service -> do
  putStrLn $ "Starting service " ++ (serviceName service)
  runService service
  putStrLn "------"
