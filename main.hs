import qualified Data.List as L

-- record syntax
type Name = String
type Year = Int
data Person = Person
  {
    firstname :: Name,
    lastname :: Name,
    yearOfBirth :: Year
  }
  deriving(Show)

-- constracting a Person
blaise :: Person
blaise = Person
  {
    firstname = "Blaise",
    lastname = "Pascal",
    yearOfBirth = 1623
  }

-- record update
traise :: Person
traise = blaise { firstname = "Traise" }

people :: [Person]
people = [
           Person "Isaac" "Newton" 1643,
           Person "Leonard" "Euler" 1707,
           Person "Ada" "Lovelace" 1815,
           Person "Alan" "Turing" 1912,
           Person "Haskell" "Curry" 1900
         ]

firstAfter1900 :: Maybe Person
firstAfter1900 = L.find (\person -> yearOfBirth person >= 1990) people

firstLaterIs :: Char -> String -> Bool
firstLaterIs c "" = False
firstLaterIs c (x:_) = c == x

firstNameBeginWith :: Char -> Person -> Bool
firstNameBeginWith c p = 
   firstLaterIs c firstname'
  where firstname' = firstname p

peopleThatBeginWithL :: [Person]
peopleThatBeginWithL = filter (firstNameBeginWith 'L') people

-- mapPeople :: (Person -> String) -> [Person] -> [String]
-- mapPeople f [] = []
-- mapPeople f (x:xs) = f x : mapPeople f xs

lastnames :: [String]
lastnames = map lastname people

sortedPeopleByFirstname :: [Person]
sortedPeopleByFirstname = L.sortOn firstname people

reverseSortedLastNames :: [String]
reverseSortedLastNames = L.sortBy (\x y -> compare y x) lastnames

yearsSinceBirthAtYear :: Year -> Person -> Int
yearsSinceBirthAtYear y p = y - yearOfBirth p

allYearsSinceBirthAt2001 :: [Int]
allYearsSinceBirthAt2001 = map (yearsSinceBirthAtYear 2001) people

earliestYearOfBirth :: [Person] -> Year
earliestYearOfBirth people = minimum $ L.map yearOfBirth people

bornFirst :: [Person] -> Person
bornFirst people = 
  L.minimumBy compareBirthYears people
  where compareBirthYears x y = compare (yearOfBirth x) (yearOfBirth y)
