import ParallelLayout
import RunSequential

-- | a Pll implementation of the SHA 256 algorithm as described by
-- | http://www.iwar.org.uk/comsec/resources/cipher/sha256-384-512.pdf

type Rule = String -> String

-- | Apply a series of rules to a series of words
applyRules :: Int -> Source Rule -> Source String -> Source String
applyRules nRules rules words = (rules' -&- words') ->- belowN nRules (act (uncurry ($)))
    where
        words' = words ->- pads nRules
        rules' = act (const 0) ->- spreadSource nRules rules 

exampleApplyRules n = runSequential (applyRules 3 (act (\n -> [id, ('a':), (++"b")]!!n)) (act (\n -> ["hej", "nej"]!!n))) [n]

-- | Perform 64 rounds of the sha intermidiate round with an intermidiate representation
sha :: Int -> (String -> a) -> (a -> a) -> (a -> String) -> Pll String String
sha nPar toRep shaRound fromRep = belowN nPar ((act toRep) ->- (besidesN 64 (act shaRound)) ->- (act fromRep))

-- | Perform sha and send the original string through
shaBlock :: Int -> (String -> a) -> (a -> a) -> (a -> String) -> Pll String (String, String)
shaBlock nPar toRep shaRound fromRep = sha nPar toRep shaRound fromRep -&- pads nPar

-- | Lookup the result of a computation in a dictionary
lookupResults :: Int -> [String] -> Pll a (String, String) -> Pll a (Maybe String)
lookupResults nPar dict src = src ->- act helper
    where
        helper (encr, plain) = if encr `elem` dict then Just plain else Nothing
