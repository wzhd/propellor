{-# LANGUAGE DataKinds #-}
module DefinedServices where

import Propellor.Resources
import Control.Monad

type SService = Service '[] ServiceDetails

fooInstalled :: SService
fooInstalled = makeService "foo" $ putStrLn "Foo"

apacheInstalled :: Service '[Port 443] ServiceDetails
apacheInstalled = makeService "apache" $ putStrLn "Apache service"

torBridge :: Service '[Port 80, Port 443] ServiceDetails
torBridge = makeService "tor" $ putStrLn "Tor service"

httpService :: Service '[Port 80, Port 8080] ServiceDetails
httpService = makeService "http" $ putStrLn "Http service"

serviceList1 :: [ServiceDetails]
serviceList1 = getServices $
               noServices `addService` httpService `addService` apacheInstalled `addService` fooInstalled

--serviceList2 :: [ServiceDetails]
--serviceList2 = getServices $
--               noServices `addService` apacheInstalled `addService` torBridge


main = startServices serviceList1
