{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Data.PSQ.Class.Tests
    ( tests
    ) where

import           Control.Applicative   ((<$>))
import           Control.DeepSeq       (NFData, rnf)
import           Data.Char             (isAlphaNum, isPrint, ord, toLower)
import           Data.Foldable         (Foldable, foldr)
import qualified Data.List             as List
import           Data.Tagged           (Tagged (..), untag)
import           Prelude               hiding (foldr, lookup, map, null)

import           Test.HUnit            (Assertion, assert, (@?=))
import           Test.QuickCheck       (Arbitrary (..), Property, forAll, (==>))
import           Test.Tasty            (TestTree)
import           Test.Tasty.HUnit      (testCase)
import           Test.Tasty.QuickCheck (testProperty)

import           Data.PSQ.Class
import           Data.PSQ.Class.Gen
import           Data.PSQ.Class.Util


--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Foldable (psq Int),
                    Functor (psq Int),
                    NFData (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq [TestTree]
tests = Tagged
    [ testCase "rnf"      (untag' test_rnf)
    , testCase "equality" (untag' test_equality)
    , testCase "size"     (untag' test_size)
    , testCase "size2"    (untag' test_size2)
    , testCase "empty"    (untag' test_empty)
    , testCase "lookup"   (untag' test_lookup)
    , testCase "findMin"  (untag' test_findMin)
    , testCase "alter"    (untag' test_alter)
    , testCase "alterMin" (untag' test_alterMin)
    , testCase "fromList" (untag' test_fromList)
    , testCase "foldr"    (untag' test_foldr)

    , testProperty "show"               (untag' prop_show)
    , testProperty "rnf"                (untag' prop_rnf)
    , testProperty "size"               (untag' prop_size)
    , testProperty "singleton"          (untag' prop_singleton)
    , testProperty "memberLookup"       (untag' prop_memberLookup)
    , testProperty "insertLookup"       (untag' prop_insertLookup)
    , testProperty "insertDelete"       (untag' prop_insertDelete)
    , testProperty "insertDeleteView"   (untag' prop_insertDeleteView)
    , testProperty "deleteNonMember"    (untag' prop_deleteNonMember)
    , testProperty "deleteMin"          (untag' prop_deleteMin)
    , testProperty "alter"              (untag' prop_alter)
    , testProperty "alterMin"           (untag' prop_alterMin)
    , testProperty "toList"             (untag' prop_toList)
    , testProperty "keys"               (untag' prop_keys)
    , testProperty "insertView"         (untag' prop_insertView)
    , testProperty "deleteView"         (untag' prop_deleteView)
    , testProperty "map"                (untag' prop_map)
    , testProperty "unsafeMapMonotonic" (untag' prop_unsafeMapMonotonic)
    , testProperty "fmap"               (untag' prop_fmap)
    , testProperty "fold'"              (untag' prop_fold')
    , testProperty "foldr"              (untag' prop_foldr)
    , testProperty "valid"              (untag' prop_valid)
    , testProperty "atMostView"         (untag' prop_atMostView)
    ]
  where
    untag' :: Tagged psq test -> test
    untag' = untag


--------------------------------------------------------------------------------
-- HUnit tests
--------------------------------------------------------------------------------

test_rnf
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    NFData (psq Int Char))
    => Tagged psq Assertion
test_rnf = Tagged $
    rnf (empty :: psq Int Char) `seq` return ()

test_equality
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Eq (psq Int Char))
    => Tagged psq Assertion
test_equality = Tagged $ do
    -- Mostly to get 100% coverage
    assert $ e /= s
    assert $ s /= e
  where
    e = empty               :: psq Int Char
    s = singleton 3 100 'a' :: psq Int Char

test_size
    :: forall psq. (PSQ psq, TestKey (Key psq))
    => Tagged psq Assertion
test_size = Tagged $ do
    null (empty               :: psq Int Char) @?= True
    null (singleton 1 100 'a' :: psq Int Char) @?= False

test_size2
    :: forall psq. (PSQ psq, TestKey (Key psq))
    => Tagged psq Assertion
test_size2 = Tagged $ do
    size (empty               :: psq Int ())   @?= 0
    size (singleton 1 100 'a' :: psq Int Char) @?= 1
    size (fromList [(1, 100, 'a'), (2, 101, 'c'), (3, 102, 'b')]
                              :: psq Int Char) @?= 3

test_empty
    :: forall psq. (PSQ psq, TestKey (Key psq))
    => Tagged psq Assertion
test_empty = Tagged $ do
    toList (empty :: psq Int ())   @?= []
    size   (empty :: psq Char Int) @?= 0

test_lookup
    :: forall psq. (PSQ psq, TestKey (Key psq))
    => Tagged psq Assertion
test_lookup = Tagged $ do
    employeeCurrency 1 @?= Just 1
    employeeCurrency 2 @?= Nothing
  where
    employeeDept    = fromList [(1, 100, 2), (3, 101, 1)] :: psq Int Int
    deptCountry     = fromList [(1, 102, 1), (2, 103, 2)] :: psq Int Int
    countryCurrency = fromList [(1, 104, 2), (2, 105, 1)] :: psq Int Int

    employeeCurrency :: Int -> Maybe Int
    employeeCurrency name = do
        dept    <- snd <$> lookup (toTestKey name) employeeDept
        country <- snd <$> lookup (toTestKey dept) deptCountry
        snd <$> lookup (toTestKey country) countryCurrency

test_findMin
    :: forall psq. (PSQ psq, TestKey (Key psq))
    => Tagged psq Assertion
test_findMin = Tagged $ do
    findMin (empty :: psq Int Char) @?= Nothing
    findMin (fromList [(5, 101, 'a'), (3, 100, 'b')] :: psq Int Char) @?=
        Just (3, 100, 'b')

test_alter
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Assertion
test_alter = Tagged $ do
    alter f 3 (empty :: psq Int Char) @?= ("Hello", singleton 3 100 'a')
    alter f 3 (singleton 3 100 'a' :: psq Int Char) @?= ("World", empty)
    alter f 3 (singleton 3 100 'b' :: psq Int Char) @?=
        ("Cats", singleton 3 101 'b')
  where
    f Nothing           = ("Hello", Just (100, 'a'))
    f (Just (100, 'a')) = ("World", Nothing)
    f (Just _)          = ("Cats",  Just (101, 'b'))

test_alterMin
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Assertion
test_alterMin = Tagged $ do
    alterMin (\_ -> ((), Nothing)) (empty :: psq Int Char) @?= ((), empty)
    alterMin (\_ -> ((), Nothing)) (singleton 3 100 'a'  :: psq Int Char) @?=
        ((), empty)

test_fromList
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Assertion
test_fromList = Tagged $ do
    let ls = [(1, 0, 'A'), (2, 0, 'B'), (3, 0, 'C'), (4, 0, 'D')]
      in (fromList ls :: psq Int Char) @?= fromList (reverse ls)
    let qs = [(4, 0, 'D'), (1, 5, 'Q'), (2, 0, 'B'), (1, 0, 'A'), (3, 0, 'C'), (2, 5, 'Z')]
        qs' = [(1, 0, 'A'), (2, 5, 'Z'), (3, 0, 'C'), (4, 0, 'D')]
      in List.sort (toList (fromList qs :: psq Int Char)) @?= qs'

test_foldr
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Foldable (psq Int))
    => Tagged psq Assertion
test_foldr = Tagged $
    foldr (\x acc -> acc + ord x) 0 (empty :: psq Int Char) @?= 0


--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

-- | For 100% test coverage...
prop_show
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Show (psq Int Char))
    => Tagged psq Property
prop_show = Tagged $
    forAll arbitraryPSQ $ \t ->
        length (coverShowInstance (t :: psq Int Char)) > 0

-- | For 100% test coverage...
prop_rnf
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    NFData (psq Int Char), Show (psq Int Char))
    => Tagged psq Property
prop_rnf = Tagged $
    forAll arbitraryPSQ $ \t ->
        rnf (t :: psq Int Char) `seq` True

prop_size
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_size = Tagged $ \t ->
    size (t :: psq Int Char) == length (toList t)

prop_singleton
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Eq (psq Int Char))
    => Tagged psq Property
prop_singleton = Tagged $
    forAll arbitraryTestKey  $ \k ->
    forAll arbitraryPriority $ \p ->
    forAll arbitrary         $ \x ->
        insert k p x empty == (singleton k p x :: psq Int Char)

prop_memberLookup
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_memberLookup = Tagged $ \t ->
    forAll arbitraryTestKey $ \k ->
        case lookup k (t :: psq Int Char) of
            Nothing -> not (member k t)
            Just _  -> member k t

prop_insertLookup
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_insertLookup = Tagged $ \t ->
    forAll arbitraryTestKey  $ \k ->
    forAll arbitraryPriority $ \p ->
    forAll arbitrary         $ \c ->
        lookup k (insert k p c (t :: psq Int Char)) == Just (p, c)

prop_insertDelete
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_insertDelete = Tagged $ \t ->
    forAll arbitraryTestKey  $ \k ->
    forAll arbitraryPriority $ \p ->
    forAll arbitrary         $ \c ->
        (lookup k t == Nothing) ==>
            (delete k (insert k p c t) == (t :: psq Int Char))

prop_insertDeleteView
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_insertDeleteView = Tagged $ \t ->
    forAll arbitraryTestKey  $ \k ->
    forAll arbitraryPriority $ \p ->
    forAll arbitrary         $ \c ->
        case deleteView k (insert k p c (t :: psq Int Char)) of
            Nothing           -> False
            Just (p', c', t')
                | member k t -> p' == p && c' == c && size t' < size t
                | otherwise  -> p' == p && c' == c && t' == t

prop_deleteNonMember
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_deleteNonMember = Tagged $ \t ->
    forAll arbitraryTestKey $ \k ->
        (lookup k t == Nothing) ==> (delete k t == (t :: psq Int Char))

prop_deleteMin
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_deleteMin = Tagged $ \t ->
    let t' = deleteMin t
    in if null t
        then t' == t
        else case findMin t of
                Nothing        -> False
                Just (k, _, _) ->
                    size t' == size t - 1 && member k t && not (member k t')

prop_alter
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_alter = Tagged $ \t ->
    forAll arbitraryTestKey $ \k ->
        let ((), t') = alter f k t :: ((), psq Int Char)
        in case lookup k t of
            Just _  -> (size t - 1) == size t' && lookup k t' == Nothing
            Nothing -> (size t + 1) == size t' && lookup k t' /= Nothing
  where
    f Nothing  = ((), Just (100, 'a'))
    f (Just _) = ((), Nothing)

prop_alterMin
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_alterMin = Tagged $ \t ->
    let (mbMin, t') = alterMin f (t :: psq Int Char)
    in case mbMin of
        Nothing        -> t' == singleton 3 100 'a'
        Just (k, p, v) ->
            findMin t == Just (k, p, v) &&
            member k t &&
            (case () of
                _ | isAlphaNum v -> lookup k t' == Just (fromTestKey k, v)
                  | isPrint v    -> lookup (toTestKey $ ord v) t' ==
                                        Just (ord v, v)
                  | otherwise    -> not (member k t'))
  where
    f Nothing          = (Nothing, Just (3, 100, 'a'))
    f (Just (k, p, v))
        | isAlphaNum v = (Just (k, p, v), Just (k, fromTestKey k, v))
        | isPrint v    = (Just (k, p, v), Just (toTestKey (ord v), ord v, v))
        | otherwise    = (Just (k, p, v), Nothing)

prop_toList
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_toList = Tagged $ \t ->
    (t :: psq Int Char) == fromList (toList t)

prop_keys
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_keys = Tagged $ \t ->
    List.sort (keys (t :: psq Int Char)) ==
        List.sort [k | (k, _, _) <- toList t]

prop_insertView
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_insertView = Tagged $ \t ->
    forAll arbitraryTestKey  $ \k ->
    forAll arbitraryPriority $ \p ->
    forAll arbitrary         $ \x ->
        case insertView k p x (t :: psq Int Char) of
            (mbPx, t') ->
                lookup k t  == mbPx && lookup k t' == Just (p, x)

prop_deleteView
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_deleteView = Tagged $ \t ->
    forAll arbitraryTestKey $ \k ->
        case deleteView k (t :: psq Int Char) of
            Nothing         -> not (member k t)
            Just (p, v, t') -> lookup k t == Just (p, v) && not (member k t')

prop_map
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_map = Tagged $ \t ->
    map f (t :: psq Int Char) ==
        fromList (List.map (\(k, p, x) -> (k, p, f k p x)) (toList t))
  where
    f k p x = if fromEnum k > p then x else 'a'

prop_unsafeMapMonotonic
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_unsafeMapMonotonic = Tagged $ \t ->
    let t' = unsafeMapMonotonic f (t :: psq Int Char) :: psq Int Char in
    valid t' &&
    t' == fromList (List.map (\(k, p, x) -> let (p', x') = f k p x in (k, p', x'))
                           (toList t))
  where
    f k p x = (p + 1, if fromEnum k > p then x else 'a')

prop_fmap
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Eq (psq Int Char),
                    Functor (psq Int),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_fmap = Tagged $ \t ->
    fmap toLower (t :: psq Int Char) ==
        fromList (List.map (\(p, v, x) -> (p, v, toLower x)) (toList t))

prop_fold'
    :: forall psq. (PSQ psq, TestKey (Key psq),
                    Arbitrary (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_fold' = Tagged $ \t ->
    fold' f acc0 (t :: psq Int Char) ==
        List.foldl' (\acc (k, p, x) -> f k p x acc) acc0 (toList t)
  where
    -- Needs to be commutative
    f k p x (kpSum, xs) = (kpSum + fromEnum k + p, List.sort (x : xs))
    acc0                = (0, [])

prop_foldr
    :: forall psq. (PSQ psq,
                    Arbitrary (psq Int Char),
                    Foldable (psq Int),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_foldr = Tagged $ \t ->
    foldr f 0 (t :: psq Int Char) ==
        List.foldr (\(_, _, x) acc -> f x acc) 0 (toList t)
  where
    f x acc = acc + ord x

prop_valid
    :: forall psq. (PSQ psq,
                    Arbitrary (psq Int Char),
                    Show (psq Int Char))
    => Tagged psq (psq Int Char -> Bool)
prop_valid = Tagged valid

prop_atMostView
    :: forall psq. (PSQ psq, Show (Key psq), Show (psq Int Char))
    => Tagged psq (psq Int Char -> Property)
prop_atMostView = Tagged $ \t ->
    forAll arbitraryPriority $ \p ->
        let (elems, t') = atMostView p t in
        -- 1. Test that priorities are at most 'p'.
        and [p' <= p | (_, p', _) <- elems] &&
        -- 2. Test that the remaining priorities are larger than 'p'.
        (case findMin t' of
            Nothing         -> True
            Just (_, p', _) -> p' > p) &&
        -- 2. Test that the size of the removed elements and the new queue total
        -- the original size.
        length elems + size t' == size t
