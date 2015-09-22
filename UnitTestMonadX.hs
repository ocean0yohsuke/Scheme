-- {-# LANGUAGE ScopedTypeVariables #-}
module UnitTestMonadX (
	run
	) where 
import Test.HUnit

import MonadX.Applicative
import MonadX.Monad
import MonadX.Arrow
import MonadX.Monad.RWS
import MonadX.Monad.Error

import System.IO.Unsafe (unsafePerformIO)


-- | Run Unit-Test of this module. 
run = runTestTT $ TestList 
        [ TestList testCase_MonadX_Applicative
        , TestList testCase_MonadX_Monad
        --, TestList testCase_MonadX_Arrow


    	]

-- *UnitTest> run

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

testCase_MonadX_Applicative = (\t -> "MonadX" ~: "Applicative" ~: t) |$>
    [ 1 <|(+)|> 2               ~?= 3
    , 1 <|(+)|> 2 <|(-)|> 3     ~?= 0
    , 1 <|(+)|> 2 <|(*)|> 3     ~?= 9

    , 1 <|(+)|$> [2]      ~?= [3]
    , [1] <$|(+)|* 2      ~?= [3]
    , (+) |$> [1] |*> [2]      ~?= [3]
    , [1] <$|(+)|*> [2]        ~?= [3]

    , 1 <|(,)|$> Just 2 ~?= Just (1,2)
    , Just 1 <$|(,)|* 2 ~?= Just (1,2)

    , [1] <$|(+)|*> [2]                ~?= [3]
    , [1] <$|(+)|*> [2] <$|(-)|*> [3]  ~?= [0]
    , [1] <$|(+)|* 2             ~?= [3]
    , [1] <$|(+)|* 2 <$|(-)|* 3  ~?= [0]

    , [Just 1] <<$|(+)|*>> [Just 2]              ~?= [Just 3]
    , [[1]] <<$|(+)|*>> [[2]] <<$|(-)|*>> [[3]]  ~?= [[0]]
    , [Just 1] <<$|(+)|** 2              ~?= [Just 3]
    , [Just 1] <<$|(+)|*- (Just 2)       ~?= [Just 3]
    , [Just 1] <<$|(+)|-* [2]            ~?= [Just 3]
    , [[1]] <<$|(+)|** 2 <<$|(-)|** 3    ~?= [[0]]

    , (1+) |$> (1+) |$> Just 1 ~?= Just 3
    , (fmap (1+) $ (1+) |$> Just 1) ~?= Just 3

-- invalid forms
-- [1] <$|(+)|> 2   
-- ([2] <*| [1] <$| (+))  
-- ([2] <*|(+)|$> [1])        
-- ([3] <*|(+)|$> [2] <*|(-)|$>)   

    ]


testCase_MonadX_Monad = (\t -> "MonadX" ~: "Monad" ~: t) |$>
    [ (3 >- Just)                         ~?= (Just 3)

    , (3 >- ((\a -> Just (a*2)) >=> (\a -> Just (a+1))))  ~?= Just 7
    , (((\a -> Just (a*2)) >=> (\a -> Just (a+1))) -< 3)  ~?= Just 7

    , (1 >- (+1) >- (*2) >- (+3)) ~?= 7
    , (1 <| (+1) <| (*2) <| (+3)) ~?= 7
    , (1 >- ((+1) >>> (*2) >>> (+3))) ~?= 7
    , (1 <| ((+1) >>> (*2) >>> (+3))) ~?= 7

    , (1 <| (+1) >- (*2)) ~?= 4

    , (((3+) <<< (2*) <<< (1+)) -< 1) ~?= 7
    , (((3+) <<< (2*) <<< (1+)) |> 1) ~?= 7

-- invalid forms
-- ((3+) -< (2*) -< (1+) -< 1)
-- ((3+) |> (2*) |> (1+) |> 1)

    ]

{-
testCase_MonadX_Arrow = (\t -> "MonadX" ~: "Arrow" ~: t) |$>
    [ 
    ]
-}

