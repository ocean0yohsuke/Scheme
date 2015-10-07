module Util.Bits (
    Bit(..), Bits(..), Byte,
    bitToInt, bitToInteger, bitsToInteger,
    toByte, toBinary
) where

import DeepControl.Applicative

data Bit = Zero | One
  deriving (Show, Eq)
newtype Bits = Bits [Bit]
  deriving (Eq)

type Byte = Bits

instance Show Bits where
    show (Bits [])        = ""
    show (Bits (Zero:xs)) = "0" ++ show (Bits xs)
    show (Bits (One:xs))  = "1" ++ show (Bits xs)

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One  = 1
bitToInteger :: Bit -> Integer
bitToInteger Zero = 0
bitToInteger One  = 1
bitsToInteger :: Bits -> Maybe Integer
bitsToInteger (Bits [])   = Nothing
bitsToInteger (Bits bits) = Just . (foldl (+) 0) . (zipWith (*) powers) . reverse $ bitToInteger |$> bits

powers :: [Integer]
powers = (2^) |$> [0..]

toByte :: Int -> Maybe Byte
toByte n | 255 >= n && n >= 0 = Just $ iter (toInteger n) [] 7
         | otherwise          = Nothing
  where
    iter :: Integer -> [Bit] -> Int -> Bits
    iter 0 bits 0 = Bits (bits ++ [Zero])
    iter 1 bits 0 = Bits (bits ++ [One])
    iter n bits i = 
        if   powers!!(i+1) > n && n >= powers!!i
        then iter (n - powers!!i) (bits ++ [One]) (i-1)
        else iter n (bits ++ [Zero]) (i-1)   

toBinary :: Int -> Maybe Bits
toBinary n | n < 0     = Nothing
           | otherwise = Just $ iter (toInteger n) [] order
  where
    order :: Int
    order = iter 0
      where
        iter :: Int -> Int
        iter i = 
            if   powers!!(i+1) > (toInteger n)
            then i
            else iter (i+1)
    iter :: Integer -> [Bit] -> Int -> Bits
    iter 0 bits 0 = Bits (bits ++ [Zero])
    iter 1 bits 0 = Bits (bits ++ [One])
    iter n bits i = 
        if   n >= powers!!i
        then iter (n - powers!!i) (bits ++ [One]) (i-1)
        else iter n (bits ++ [Zero]) (i-1)   


