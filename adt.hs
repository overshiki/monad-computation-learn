{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Complex
import System.Random (StdGen, Random)
import qualified Data.Random as Ext

-- data Dist a = Return a 
--             | Bind (Dist b) (b -> Dist a)
--             | Primitive (d a)
--             | Conditional (Dist a)

newtype Prob = Prob {toDouble :: Double}
    deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Floating, Random, Ext.Distribution Ext.StdUniform)

class Sampleable d where
    sample :: StdGen -> d a -> a


data Dist a where
    Return      :: a -> Dist a
    Bind        :: Dist b -> (b -> Dist a) -> Dist a
    Primitive   :: (Sampleable d) => d a -> Dist a
    Conditional :: (a -> Prob) -> Dist a -> Dist a


type RR = Double
type CC = Complex RR
newtype Qbit = Qbit Int deriving (Num, Enum, Eq, Ord)
type Rotation = ((Bool,Bool) -> CC)

data U = UReturn 
        | Rot Qbit Rotation U
        | Swap Qbit Qbit U 
        | Cond Qbit (Bool -> U) U 
        | Ulet Bool (Qbit -> U) U

type RotSymbol = String

data Uni a where 
    UniReturn :: a -> Uni a 
    UniRot    :: a -> RotSymbol -> Uni a 
    UniSwap   :: a -> a -> Uni a 
    UniCond   :: a -> (Bool -> Uni a) -> Uni a 
    UniLet    :: Bool -> (a -> Uni a) -> Uni a






main :: IO ()
main = do 
    print "here"