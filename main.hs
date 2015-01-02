{-# LANGUAGE DataKinds #-}
import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set



myFirstMaybe :: Maybe Int -> String
myFirstMaybe mt = case mt of
  Nothing -> "Nothing!"
  Just t  -> "Something! " ++ (show t)



type HeroRatings = (Map.Map String Float)
type HeroDB      = (Map.Map String HeroRatings)

testHeroCounters :: HeroDB
testHeroCounters =
  Map.fromList
  [  ("Axe",               Map.fromList [("Outworld Devourer", 1.5),
                                         ("Lina",              2.5)])
   , ("Outworld Devourer", Map.fromList [("Nyx Assassin", 3.5),
                                         ("Lina",         1.5)])
   , ("Nyx Assassin",      Map.fromList [("Axe", 1.0),
                                         ("Lina", 4.5)])
   , ("Lina",              Map.fromList [("Axe", 4.5),
                                         ("Outworld Devourer", 1.5)])
   , ("Drow Ranger",       Map.fromList [("Lina", 3.5),
                                         ("Axe", 3.5)])
   ]

valueAgainst :: HeroDB -> String -> String -> Float
valueAgainst db us them =
  let maybeRating = Map.lookup us db
  in (case maybeRating of
       Just ratings -> (let maybeScore = Map.lookup them ratings
                        in
                         case maybeScore of
                          Just x  -> x
                          Nothing -> 2.5)
       Nothing    -> 2.5)



data RadiantOrDire = Radiant | Dire deriving (Show, Eq)

data Player =
  Player {
      steam_id :: String
    } deriving (Show, Eq)

instance Ord Player where
  (Player s1) `compare` (Player s2) = s1 `compare` s2

data Game =
  Game {
      radiant :: Map.Map Player String
    , dire    :: Map.Map Player String
    , victor  :: RadiantOrDire
    } deriving (Show)



-- Cool Kids
_arrdem    = Player "arrdem"
_bitemyapp = Player "diogenes"
_cube      = Player "Charming_Prisim"
_fox       = Player "Frozenfoxx"
_ting      = Player "_wting"
coolKids   = [_arrdem, _bitemyapp, _cube, _fox, _ting]

-- The Pubs
_p = Player "[MLG] MidOrFeed"
_q = Player "[EG] SoloMid"
_r = Player "-AfK- PudgeOrFeed"
_s = Player "|++0--|"
_t = Player "YourMother"
pugs = [_p, _q, _r, _s, _t]



testGameHistories :: [Game]
p1 = ["Axe",   "Clinx",  "Bane",  "Witchdoctor", "Sven"]
p2 = ["Pudge", "Sniper", "Meepo", "Oracle",      "Spirit Breaker"]
p3 = ["Lina",  "Clinx",  "Bane",  "Dazzle",      "Bristleback"]

g1 = (Game
      (Map.fromList (zipWith (,) coolKids p1))
      (Map.fromList (zipWith (,) pugs     p2))
      Radiant)

g2 = (Game
      (Map.fromList (zipWith (,) coolKids p1))
      (Map.fromList (zipWith (,) pugs     p1))
      Dire)

g3 = (Game
      (Map.fromList (zipWith (,) coolKids p3))
      (Map.fromList (zipWith (,) pugs     p2))
      Radiant)


-- Lina is totally OP. Totally. Not fixing these stats at all. Nope. Why would you say that.
testGameHistories = [g1, g2, g2, g2, g3, g3, g3, g3, g3, g3]

-- Can be cute about computing final scores in parallel with this
data AccValue =
  AccValue {
      games  :: Int
    , record :: Map.Map String Int
    }

mergeACCs :: AccValue -> AccValue -> AccValue
mergeACCs l r =
  AccValue ((games l) + (games r))
           (Map.unionWith (+) (record l) (record r))

type Leaf = (Map.Map String AccValue)

mergeLeafs :: Leaf -> Leaf -> Leaf
mergeLeafs l r =
  Map.unionWith mergeACCs l r

leafFromHeros :: [String] -> [String] -> Int -> Leaf
leafFromHeros as bs c =
  let inner_map = Map.fromList (map (\x -> (x, c)) bs)
  in Map.fromList (map (\y -> (y, (AccValue 1 inner_map))) as)

gameToLeafs :: Game -> [Leaf]
gameToLeafs game =
  let (rs, ds) = (if ((victor game) == Radiant)
                  then (5,0) else (0,5))
      rHeros = Map.elems (radiant game)
      dHeros = Map.elems (dire game)
  in [(leafFromHeros rHeros dHeros rs),
      (leafFromHeros dHeros rHeros ds)]

dbFromLeaf :: Leaf -> HeroDB
dbFromLeaf l =
  (Map.map (\x -> Map.map (\y -> ((fromIntegral y) / (fromIntegral (games x))))
                          (record x))
           l)

dbFromList :: [Game] -> HeroDB
dbFromList games = -- FIXME: this is trivially parallelizable
  (dbFromLeaf . (foldr mergeLeafs Map.empty) . (concatMap gameToLeafs)) games
