module Sample where

import Text.Printf
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Exception (assert)
import Coverage

g = 1

z :: Int -> Int
z a = (z' a) +  (z'' a) + (z''' a)

z' :: Int -> Int
z' a = if a > 2 then a else a

z'' :: Int -> Int
z'' 1 = 2
z'' 3 = 0
z'' _ = 0

z''' :: Int -> Int
z''' a = case a of 
    1 -> 2
    2 -> 3
    _ -> 0

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)
--Cannot Do This Therefore All of Function Declarations Are In Same Fun Bind Block
--z'' 4 = 5
--z'  1 = 5
--z'' 2 = 5

type Quantity = Int
type Price = Int
type TimeStamp = Int
type OrderID = Int
type BrokerID = Int
type ShareholderID = Int
type CreditInfo = Map.Map BrokerID Int
type OwnershipInfo = Map.Map ShareholderID Int

data Side = Buy | Sell deriving (Show, Eq, Ord)

data Order = LimitOrder 
  { oid :: OrderID
  , brid :: BrokerID
  , shid :: ShareholderID
  , price :: Price
  , quantity :: Quantity
  , side :: Side
  , minQty :: Maybe Quantity
  , fillAndKill :: Bool
  } | IcebergOrder 
  { oid :: OrderID
  , brid :: BrokerID
  , shid :: ShareholderID
  , price :: Price
  , quantity :: Quantity
  , side :: Side
  , minQty :: Maybe Quantity
  , fillAndKill :: Bool
  , disclosedQty :: Quantity
  , peakSize :: Quantity
  } deriving (Show, Eq)

type OrderQueue = [Order]

data OrderBook = OrderBook 
  { buyQueue :: OrderQueue
  , sellQueue :: OrderQueue
  } deriving (Show, Eq)

queue :: Side -> OrderBook -> OrderQueue
queue Buy ob = buyQueue ob
queue Sell ob = sellQueue ob

data Trade = Trade 
  { priceTraded :: Price
  , quantityTraded :: Quantity
  , buyId :: OrderID
  , sellId :: OrderID
  , buyerShId :: ShareholderID
  , buyerBrId :: BrokerID
  , sellerShId :: ShareholderID
  , sellerBrId :: BrokerID
  } deriving (Show, Eq)

data MEState = MEState
  { orderBook :: OrderBook
  , creditInfo :: CreditInfo
  , ownershipInfo :: OwnershipInfo
  } deriving (Show, Eq)

initMEState :: MEState
initMEState = MEState (OrderBook [] []) Map.empty Map.empty

data Request = NewOrderRq
  { order :: Order
  } | CancelOrderRq 
  { orderIDToCancel :: OrderID
  } | SetCreditRq 
  { broker :: BrokerID
  , credit :: Int
  } | SetOwnershipRq
  { shareholder :: ShareholderID
  , shares :: Int
  } deriving (Show, Eq)

data Response = NewOrderRs
  { success :: Bool
  , trades :: [Trade]
  } | CancelOrderRs 
  { success :: Bool
  } | SetCreditRs
  { success :: Bool
  } | SetOwnershipRs
  { success :: Bool
  } deriving (Show, Eq)

type Handler = Request -> MEState -> Coverage (Response, MEState)
type Decorator = Handler -> Handler

valueTraded :: Trade -> Int
valueTraded t = (priceTraded t) * (quantityTraded t)

limitOrder i bi shi p q s m fak =
  assert (i >= 0) $
  assert (p > 0) $
  assert (q > 0) $
  case m of {(Just mq) -> assert (mq > 0); otherwise -> id} $
  LimitOrder i bi shi p q s m fak

icebergOrder i bi shi p q s m fak dq ps =
  assert (i >= 0) $
  assert (p > 0) $
  assert (q >= 0) $
  case m of {(Just mq) -> assert (mq > 0); otherwise -> id} $
  assert (dq <= q) $
  assert (ps > 0) $
  IcebergOrder i bi shi p q s m fak dq ps

totalQuantity :: Side -> ShareholderID -> OrderBook -> Quantity
totalQuantity side shi ob =
  sum $
  map quantity $
  filter (\o -> shid o == shi) $
  queue side ob

decQty :: Order -> Quantity -> Order
decQty (LimitOrder i bi shi p q s mq fak) q' = limitOrder i bi shi p (q - q') s mq fak
decQty (IcebergOrder i bi shi p q s mq fak dq ps) q' = icebergOrder i bi shi p (q - q') s mq fak (dq -q') ps

removeOrderFromQueue :: OrderID -> OrderQueue -> OrderQueue
removeOrderFromQueue _ [] = []
removeOrderFromQueue idToRemove (o:os)
  | oid o == idToRemove = os
  | oid o /= idToRemove = o:(removeOrderFromQueue idToRemove os) 

removeOrderFromOrderBook :: OrderID -> OrderBook -> OrderBook
removeOrderFromOrderBook idToRemove (OrderBook bq sq) = 
  OrderBook (removeOrderFromQueue idToRemove bq) (removeOrderFromQueue idToRemove sq)

removeOrderFromState :: OrderID -> MEState -> MEState
removeOrderFromState idToRemove s =
  s { orderBook = removeOrderFromOrderBook idToRemove (orderBook s) }

findOrder :: OrderID -> MEState -> Maybe Order
findOrder idToFind s = 
  case List.find (\o -> oid o == idToFind) (buyQueue (orderBook s)) of
    Just o -> Just o
    Nothing -> List.find (\o -> oid o == idToFind) (sellQueue (orderBook s))

queuesBefore :: Order -> Order -> Bool
queuesBefore o o'
  | (side o == Sell) && (side o' == Sell) = (price o < price o')
  | (side o == Buy) && (side o' == Buy) = (price o > price o')
  | otherwise = error "incomparable orders"

enqueueOrder :: Order -> OrderQueue -> OrderQueue
enqueueOrder (IcebergOrder i bi shi p q s mq fak dq ps) =
  enqueueOrder' (IcebergOrder i bi shi p q s mq fak (min q ps) ps)
enqueueOrder (LimitOrder i bi shi p q s mq fak) =
  enqueueOrder' (LimitOrder i bi shi p q s mq fak)

enqueueOrder' :: Order -> OrderQueue -> OrderQueue
enqueueOrder' o [] = [o]
enqueueOrder' o (o1:os)
  | queuesBefore o o1 = o:(o1:os)
  | otherwise = o1:(enqueueOrder' o os)

enqueue :: Order -> OrderBook -> OrderBook
enqueue o ob
  | side o == Buy = OrderBook (enqueueOrder o $ buyQueue ob) (sellQueue ob)
  | side o == Sell = OrderBook (buyQueue ob) (enqueueOrder o $ sellQueue ob) 

enqueueIcebergRemainder :: OrderQueue -> Order -> Coverage OrderQueue
enqueueIcebergRemainder os (IcebergOrder _ _ _ _ 0 _ _ _ _ _) = os `covers` "EIR-1"
enqueueIcebergRemainder os (IcebergOrder i bi shi p q s mq fak 0 ps)
  | q <= ps = enqueueOrder (icebergOrder i bi shi p q s mq fak q ps) os `covers` "EIR-2"
  | otherwise = enqueueOrder (icebergOrder i bi shi p q s mq fak ps ps) os `covers` "EIR-3"

matchBuy :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
matchBuy o [] = (Just o, [], []) `covers` "MB-0"
matchBuy o oq@((LimitOrder i1 bi1 shi1 p1 q1 s1 mq1 fak):os)
  | p < p1 = (Just o, oq, []) `covers` "MBL-1"
  | q < q1 = (Nothing, (decQty (head oq) q):os, [Trade p1 q i i1 shi bi shi1 bi1]) `covers` "MBL-2"
  | q == q1 = (Nothing, os, [Trade p1 q i i1 shi bi shi1 bi1]) `covers` "MBL-3"
  | q > q1 = do
      (o', ob', ts') <- matchBuy (decQty o q1) os
      (o', ob', (Trade p1 q1 i i1 shi bi shi1 bi1):ts') `covers` "MBL-4"
  where
    p = price o
    q = quantity o
    i = oid o
    shi = shid o
    bi = brid o

matchBuy o ((IcebergOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1 dq1 ps1):os)
  | p < p1 = (Just o, (icebergOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1 dq1 ps1):os, []) `covers` "MBI-1"
  | q < dq1 = (Nothing, (icebergOrder i1 bi1 shi1 p1 (q1-q) s1 mq1 fak1 (dq1-q) ps1):os, [Trade p1 q i i1 shi bi shi1 bi1]) `covers` "MBI-2"
  | q == dq1 = do
      newQueue <- enqueueIcebergRemainder os (icebergOrder i1 bi1 shi1 p1 (q1-dq1) s1 mq1 fak1 0 ps1)
      (Nothing, newQueue, [Trade p1 q i i1 shi bi shi1 bi1]) `covers` "MBI-3"
  | q > dq1 = do
      newQueue <- enqueueIcebergRemainder os (icebergOrder i1 bi1 shi1 p1 (q1-dq1) s1 mq1 fak1 0 ps1)
      (o', ob', ts') <- matchBuy (decQty o dq1) newQueue
      (o', ob', (Trade p1 dq1 i i1 shi bi shi1 bi1):ts') `covers` "MBI-4"
  where
    p = price o
    q = quantity o
    i = oid o
    shi = shid o
    bi = brid o

matchSell :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
matchSell o [] = (Just o, [], []) `covers` "MS-0"

matchSell o oq@((LimitOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1):os)
  | p > p1 = (Just o, oq, []) `covers` "MSL-1"
  | q < q1 = (Nothing, (decQty (head oq) q):os, [Trade p1 q i1 i shi1 bi1 shi bi]) `covers` "MSL-2"
  | q == q1 = (Nothing, os, [Trade p1 q i1 i shi1 bi1 shi bi]) `covers` "MSL-3"
  | q > q1 = do
      (o', ob', ts') <- matchSell (decQty o q1) os
      (o', ob', (Trade p1 q1 i1 i shi1 bi1 shi bi):ts') `covers` "MSL-4"
  where
    p = price o
    q = quantity o
    i = oid o
    shi = shid o
    bi = brid o

matchSell o ((IcebergOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1 dq1 ps1):os)
  | p > p1 = (Just o, (icebergOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1 dq1 ps1):os, []) `covers` "MSI-1"
  | q < dq1 = (Nothing, (icebergOrder i1 bi1 shi1 p1 (q1-q) s1 mq1 fak1 (dq1-q) ps1):os, [Trade p1 q i1 i shi1 bi1 shi bi]) `covers` "MSI-2"
  | q == dq1 = do
      newQueue <- enqueueIcebergRemainder os (icebergOrder i1 bi1 shi1 p1 (q1-dq1) s1 mq1 fak1 0 ps1)
      (Nothing, newQueue, [Trade p1 q i1 i shi1 bi1 shi bi])  `covers` "MSI-3"
  | q > dq1 = do
      newQueue <- enqueueIcebergRemainder os (icebergOrder i1 bi1 shi1 p1 (q1-dq1) s1 mq1 fak1 0 ps1)
      (o', ob', ts') <- matchSell (decQty o dq1) newQueue
      (o', ob', (Trade p1 dq1 i1 i shi1 bi1 shi bi):ts') `covers` "MSI-4"
  where
    p = price o
    q = quantity o
    i = oid o
    shi = shid o
    bi = brid o

matchNewOrder :: Order -> OrderBook -> Coverage (OrderBook, [Trade])
matchNewOrder o ob
  | side o == Buy = do
      (rem, sq, ts) <- (matchBuy o (sellQueue ob))
      case rem of
        Nothing -> (OrderBook (buyQueue ob) sq, ts) `covers` "MNO-1"
        Just o' -> (enqueue o' $ OrderBook (buyQueue ob) sq, ts) `covers` "MNO-2"
  | side o == Sell = do
      (rem, bq, ts) <- (matchSell o (buyQueue ob))
      case rem of
        Nothing -> (OrderBook bq (sellQueue ob), ts) `covers` "MNO-3"
        Just o' -> (enqueue o' $ OrderBook bq (sellQueue ob), ts) `covers` "MNO-4"