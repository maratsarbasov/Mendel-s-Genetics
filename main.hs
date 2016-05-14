module MendelInheritance (
        Genotype,
        stringToGenotype,
        getChildren
    ) where

import Data.Maybe
import Data.Char
import Data.List (sort)
import Data.String

-- | Allele data.
data Allele
    = Dominant      -- ^ Dominant
    | Recessive     -- ^ Recessive
    deriving (Show, Eq, Ord)

data Genotype = Genotype [Feature]

data Prob a = Prob [(a, Float)] deriving (Show)
instance Functor Prob where
    fmap func (Prob xs) = Prob $ map applyFunc xs
        where
            applyFunc (d, f) = (func d, f)



instance Show Genotype where
    show (Genotype fs) = foldMap show fs   -- foldl (\acc f -> acc ++ show f) [] a

instance Eq Genotype where
    (Genotype g) == (Genotype f) = g == f

instance IsString Genotype where
    fromString = stringToGenotype

data Phenotype = Phenotype [Trait] deriving (Show)

type ChildProbs = [(Genotype, Float)]

-- | Feature consists of featureSymbol and 2 alleles
data Feature = Feature { featureSymbol :: Char
                       , firstAllele   :: Allele
                       , secondAllele  :: Allele
                       } 

instance Show Feature where
    show (Feature c a1 a2) = map showA (sort [a1, a2])
        where
            showA Dominant  = toUpper c
            showA Recessive = toLower c

instance Eq Feature where
    (Feature s a1 a2) == (Feature c b1 b2) = (s == c) && ((a1 == b1 && a2 == b2) || (a1 == b2) && (a2 == b1))
    
data Trait = Trait { traitSymbol :: Char 
                   , allele :: Allele
                   } deriving (Show)

createGenotype :: [Feature] -> Genotype
createGenotype l = Genotype $ checkDuplicates l

-- TODO: 
checkDuplicates :: [Feature] -> [Feature]
checkDuplicates l = l

createAllele :: Char -> Allele
createAllele c
    | not $ isLetter c = error $ "Invalid symbol for creating Allele: " ++ [c]
    | isUpper c = Dominant
    | otherwise = Recessive


-- 
createFeature :: String -> Feature
createFeature [c1, c2] 
    | toUpper c1 == toUpper c2 = Feature { featureSymbol = toUpper c1
                                         , firstAllele   = createAllele c1
                                         , secondAllele  = createAllele c2 
                                         }
    | otherwise = error $ "Different symbols for Feature " ++ [c1, ' ', c2]
createFeature _ = error "Error while creating Feature"

getPhenotype :: Genotype -> Phenotype
getPhenotype (Genotype features) = Phenotype $ map showing features
    where 
        showing (Feature s Recessive Recessive) = Trait { traitSymbol = s, allele = Recessive}
        showing (Feature s _ _) = Trait { traitSymbol = s, allele = Dominant}

getChildren :: Genotype -> Genotype -> Prob Genotype
getChildren mother father = getChildren1 mother father


countDuplicates :: ChildProbs -> ChildProbs
countDuplicates gl = foldl (\seen x -> if isJust (lookup (fst x) seen)
                                      then addProb seen x
                                      else seen ++ [x]) [] gl
    where
        addProb tlist g = (fst g, snd g + (fromJust (lookup (fst g) tlist))) : (filter (\t -> (fst t) /= (fst g)) tlist) 


getChildren1 :: Genotype -> Genotype -> Prob Genotype
getChildren1 (Genotype mother) (Genotype father) = 
    if checkFeatures (Genotype mother) (Genotype father) then 
        sumDuplicates $ fmap Genotype $ concatProb $ zipWith f mother father 
    else 
        error "M and F feature differ"
           

f :: Feature -> Feature -> Prob Feature
f (Feature c a1 b1) (Feature _ a2 b2) = Prob [(Feature c a b, 0.25) | a <- [a1, a2], b <- [b1, b2] ]

sumDuplicates :: (Eq a) => Prob a -> Prob a
sumDuplicates (Prob gl) = Prob $ foldl (\seen x -> if isJust (lookup (fst x) seen)
                                      then addProb seen x
                                      else seen ++ [x]) [] gl
    where
        addProb tlist g = (fst g, snd g + (fromJust (lookup (fst g) tlist))) : (filter (\t -> (fst t) /= (fst g)) tlist) 


concatProb :: [Prob a] -> Prob [a]
concatProb xs = foldr (mergeProb (:)) (Prob [([],1)]) xs
    
mergeProb :: (a -> b -> c) -> Prob a -> Prob b -> Prob c
mergeProb f (Prob xs) (Prob ys)  = Prob [(f x y, px * py) | (x, px) <- xs, (y, py) <- ys]



getAlleleNumber :: Int -> Feature -> Allele
getAlleleNumber 0 a = firstAllele a
getAlleleNumber 1 a = secondAllele a

splitEvery2 :: [Int] ->[(Int, Int)]
splitEvery2 [] = []
splitEvery2 (a:b:l) = (a,b) : splitEvery2 l


 
checkFeatures :: Genotype -> Genotype -> Bool
checkFeatures (Genotype a) (Genotype b) = 
    (length a == length b) &&
    foldr (\(t1, t2) res -> res && (traitSymbol t1) == (traitSymbol t2))
        True
        (zipPhynotypes (getPhenotype (Genotype a)) (getPhenotype (Genotype b)))
    where 
        zipPhynotypes (Phenotype a) (Phenotype b) = zip a b


stringToGenotype :: String -> Genotype
stringToGenotype str = createGenotype $ map createFeature pairs 
    where 
        pairs = splitStringToPairs str

        

splitStringToPairs :: String -> [String]
splitStringToPairs ""  = []
splitStringToPairs [x] = error "Number of alleles is not even!"
splitStringToPairs str = ([str !! 0, str !! 1] : next)
    where 
        next = splitStringToPairs(tail (tail str))
