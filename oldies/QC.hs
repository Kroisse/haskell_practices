import Control.Monad ( liftM )
import Data.List ( intersperse )
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test

splitFN_0 :: String -> (String, String)
splitFN_0 fn =
  let fn' = span (/= '.') . reverse $ fn
  in case (length (fst fn') == length fn) of
    True  -> (fn, "")
    False -> (reverse . drop 1 $ snd fn', ('.':) . reverse . fst $ fn')

joinFN_0 :: (String, String) -> String
joinFN_0 (name, ext) = name ++ ext

newtype Filename = FN { unFN :: String } deriving Show

instance Arbitrary Filename where
  arbitrary = do name <- elements ["foo", "bar", "baz"]
                 ext <- listOf $ elements ['a'..'z']
                 return (FN (name ++ "." ++ ext))
                 
prop_filenames_are_roundtrippable_0 :: Filename -> Property
prop_filenames_are_roundtrippable_0 fnStr =
  property $ joinFN_0 (splitFN_0 fn) == fn
    where fn = unFN fnStr
        
prop_filenames_are_roundtrippable_1 :: Filename -> Property
prop_filenames_are_roundtrippable_1 fnStr =
  collect fn $ joinFN_0 (splitFN_0 fn) == fn
    where fn = unFN fnStr
        
prop_filenames_are_roundtrippable_2 :: Filename -> Property
prop_filenames_are_roundtrippable_2 fnStr =
  classify (length ext == 0) "no ext" $
  classify (length ext > 0 && length ext < 5) "normal ext" $
  classify (length ext >= 5) "long ext" $
  joinFN_0 (splitFN_0 fn) == fn
    where fn = unFN fnStr
          (name,ext) = splitFN_0 fn
          
filenames :: Gen String          
filenames = do
  name <- opt identifier
  dot  <- opt (return ".")
  ext  <- opt identifier
  exts <- listOf identifier
  oneof [ return $ name ++ dot ++ ext
        , return $ name ++ "." ++ (concat . intersperse "." $ exts) ]

prop_filenames_are_roundtrippable_3 :: Property
prop_filenames_are_roundtrippable_3 =
  forAll filenames $ \fn ->
  joinFN_0 (splitFN_0 fn) == fn

noExtFilenames :: Gen String
noExtFilenames = do
  name <- identifier
  dot  <- opt (return ".")
  return ( dot ++ name )

prop_names_equal_filenames_0 :: Property
prop_names_equal_filenames_0 =
  forAll noExtFilenames $ \fn ->
  let (name,ext) = splitFN_0 fn
  in name == fn

-------------------------------------------------------------------------------
-- library functions

iden0 :: Gen Char
iden0 = oneof [ elements ['a'..'z']
              , elements ['A'..'Z']
              , elements ['0'..'9'] ]
identifier :: Gen String
identifier = listOf1 iden0

opt :: Gen String -> Gen String
opt g = oneof [ g, return "" ]
