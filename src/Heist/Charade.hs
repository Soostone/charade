{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Heist.Charade
  ( charadeProductionSplice
  , charadeInit
  ) where

-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Configurator as C
import           Data.Configurator.Types
import qualified Data.Map            as M
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Text.Read
import           Heist
import           Heist.Interpreted
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           System.Random
import           Test.QuickCheck.Gen
import           Text.XmlHtml
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- | Charade needs to provide a splice that can be bound to eliminate all
-- charade-specific attributes from the markup.
charadeProductionSplice :: Monad n => Splice n
charadeProductionSplice = return . (:[]) . removeFake =<< getParamNode


removeFake :: Node -> Node
removeFake (Element tag attrs ch) = Element tag attrs' $ map removeFake ch
  where
    attrs' = filter ((/="fake") . fst) attrs
removeFake n = n


charadeSplice :: MonadIO n => M.Map Text [Text] -> HeistT n n [Node]
charadeSplice enums = do
    stdGen <- liftIO newStdGen
    (Element n attrs ch1) <- getParamNode
    let ch2 = unGen (mapM (fakeNode enums) ch1) stdGen 1
    ch3 <- runNodeList (concat ch2)
    stopRecursion
    return [Element n attrs ch3]


splices :: MonadIO n => M.Map Text [Text] -> Splices (HeistT n n [Node])
splices enums = "body" ## charadeSplice enums


------------------------------------------------------------------------------
-- | An initializer function that allows you to use charade functionality in
-- your own applications.  This is useful in situations where you have
-- implemented some of your application's functionality and you want your
-- designers to use that, but you also want them to have charade functionality
-- when working on portions of the site for which the backend has not been
-- written yet.
--
-- This is not a normal initializer.  You don't need to use nestSnaplet with
-- it.  Just call it directly from your application initializer.  When used in
-- this mode, you don't need a separate charade.cfg file.  Just put a charade
-- block in your application config, extract it with the 'subconfig' function,
-- and pass it to this function.
charadeInit :: Snaplet (Heist b) -> Config -> Initializer b v ()
charadeInit h cfg = do
    enumFiles <- liftIO $ C.lookupDefault (List []) cfg "enums"
    enumMap <- liftIO $ liftM M.fromList $ loadEnums enumFiles
    mode <- liftIO $ (C.lookup cfg "mode" :: IO (Maybe Text))
    let heistConfig = case mode of
          (Just "static") -> mempty { hcLoadTimeSplices = splices enumMap}
          (Just "dynamic") -> mempty { hcInterpretedSplices = splices enumMap}
          _ -> error "Must specify mode = 'static' or 'dynamic' in charade.cfg"

    -- Heist doesn't have a catch-all splice, and attribute splices won't work
    -- since we want to modify the actual node, so we use a load time
    -- interpreted splice attached to the body tag.
    addConfig h heistConfig


------------------------------------------------------------------------------
-- | Not sure whether we want these types specifically reified into an
-- enumeration like this or to make them dynamic strings.  At any rate, I'm
-- putting it like this now to record my thoughts on some of the types needed.
data GenTypes
  = IntT
  -- ^ Integers
  | RealT
  -- ^ Reals
  | EnumTextT
  -- ^ Text that comes from an enumeration.  This is useful for generating
  -- things like first names, last names, and other lists.
  | ChildListT
  -- ^ This type would be used on a heist tag that functions as a
  -- runChildren-style list.
  deriving (Read, Show, Eq, Enum)


------------------------------------------------------------------------------
-- | Integer generation
genInt :: [Text] -> Gen [Node]
genInt [] = genInt' 0 100
genInt [a,b] = genInt' (read $ T.unpack a) (read $ T.unpack b)
genInt _ = error "charade: invalid number of parameters to int generator"

genInt' :: Int -> Int -> Gen [Node]
genInt' a b = fmap ((:[]) . TextNode . T.pack . show) $ choose (a,b)


------------------------------------------------------------------------------
-- | Real generation
genReal :: [Text] -> Gen [Node]
genReal [] = genReal' 0 1
genReal [a,b] = genReal' (read $ T.unpack a) (read $ T.unpack b)
genReal _ = error "charade: invalid number of parameters to real generator"

genReal' :: Double -> Double -> Gen [Node]
genReal' a b = fmap ((:[]) . TextNode . T.pack . show) $ choose (a,b)


------------------------------------------------------------------------------
-- | List generation
genList :: M.Map Text [Text] -> Node -> [Text] -> Gen [Node]
genList enums node [] = genList' enums node 5
genList enums node [n] = genList' enums node $ either error fst (decimal n)
genList enums node [a,b] = do
    let minCount = either error fst (decimal a) :: Int
        maxCount = either error fst (decimal b) :: Int
    count <- choose (minCount, maxCount)
    genList' enums node count
genList _ _ _ = error "charade: invalid number of parameters to list generator"

genList' :: M.Map Text [Text] -> Node -> Int -> Gen [Node]
genList' enums node count =
    liftM concat $ vectorOf count $ liftM concat
                 $ mapM (fakeNode enums) (childNodes node)


------------------------------------------------------------------------------
-- | List generation
lorem :: Text
lorem = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

genLorem :: [Text] -> Gen [Node]
genLorem [] = return [TextNode lorem]
genLorem [n] = return [TextNode $ T.intercalate " " $ replicate count lorem]
  where
    count = either error fst (decimal n)
genLorem _ = error "charade: invalid number of parameters to lorem generator"


------------------------------------------------------------------------------
-- | Enum generation
genEnum :: [Text] -> [Node] -> Gen [Node]
genEnum [] = fmap (:[]) . elements
genEnum _ = error "charade: invalid number of parameters to enum generator"


------------------------------------------------------------------------------
-- | Enum generation
genDynEnum :: M.Map Text [Text] -> [Text] -> Gen [Node]
genDynEnum enums [file] = genEnum [] (map TextNode $ enums M.! file)
genDynEnum _ _ = error "charade: must supply an file to the enum type"


------------------------------------------------------------------------------
-- | Uses the \"fake\" attribute to determine what type of random data should
-- be generated for this node.  Usage might look something like this:
--
-- > <ul>
-- >  <personListing fake="list 5">
-- >   <li>
-- >    <firstName fake="enum firstName"/> <lastName fake="enum lastName"/>
-- >   </li>
-- >  </personListing>
-- > </ul>
fakeNode :: M.Map Text [Text] -> Node -> Gen [Node]
fakeNode enums n@(Element t a c) = case getAttribute "fake" n of
    Nothing -> do
      c' <- liftM concat $ mapM (fakeNode enums) c
      return [Element t a c']
    Just ty -> dispatchGenerator enums n (T.splitOn " " ty)
fakeNode _ n = return [n]


-- | Use the first token as the generator type and the rest of the list as
-- parameters.
dispatchGenerator :: M.Map Text [Text] -> Node -> [Text] -> Gen [Node]
dispatchGenerator _ _ [] = return []
dispatchGenerator enums node (_type:params) =
    case T.unpack _type of
      "bool"       -> genEnum params $ map TextNode ["true", "false"]
      "yesno"      -> genEnum params $ map TextNode ["yes", "no"]
      "int"        -> genInt params
      "decimal"    -> genReal params
      "list"       -> genList enums node params
      "lorem"      -> genLorem params
      "first-name" -> genEnum params $ map TextNode firstNames
      "last-name"  -> genEnum params $ map TextNode lastNames
      "enum"       -> genDynEnum enums params
      x            -> error $ "charade: Generator type " ++ x ++
                              " not recognized"


loadEnums :: Value -> IO [(Text, [Text])]
loadEnums (List files) = mapM (loadEnum . convert) files
loadEnums _ = error "charade.cfg: enums must be a list of files"


loadEnum :: Maybe FilePath -> IO (Text, [Text])
loadEnum Nothing = error "charade.cfg: all enums must be string filenames"
loadEnum (Just p) = do
    contents <- T.readFile p
    return (T.pack p, T.lines contents)


firstNames :: [Text]
firstNames =
  ["James", "John", "Robert", "Michael", "William", "David", "Richard", "Charles", "Joseph", "Thomas", "Christopher", "Daniel", "Paul", "Mark", "Donald", "George", "Kenneth", "Steven", "Edward", "Brian", "Ronald", "Anthony", "Kevin", "Jason", "Matthew", "Gary", "Timothy", "Jose", "Larry", "Jeffrey", "Frank", "Scott", "Eric", "Stephen", "Andrew", "Raymond", "Gregory", "Joshua", "Jerry", "Dennis", "Walter", "Patrick", "Peter", "Harold", "Douglas", "Henry", "Carl", "Arthur", "Ryan", "Roger", "Joe", "Juan", "Jack", "Albert", "Jonathan", "Justin", "Terry", "Gerald", "Keith", "Samuel", "Willie", "Ralph", "Lawrence", "Nicholas", "Roy", "Benjamin", "Bruce", "Brandon", "Adam", "Harry", "Fred", "Wayne", "Billy", "Steve", "Louis", "Jeremy", "Aaron", "Randy", "Howard", "Eugene", "Carlos", "Russell", "Bobby", "Victor", "Martin", "Ernest", "Phillip", "Todd", "Jesse", "Craig", "Alan", "Shawn", "Clarence", "Sean", "Philip", "Chris", "Johnny", "Earl", "Jimmy", "Antonio", "Danny", "Bryan", "Tony", "Luis", "Mike", "Stanley", "Leonard", "Nathan", "Dale", "Manuel", "Rodney", "Curtis", "Norman", "Allen", "Marvin", "Vincent", "Glenn", "Jeffery", "Travis", "Jeff", "Chad", "Jacob", "Lee", "Melvin", "Alfred", "Kyle", "Francis", "Bradley", "Jesus", "Herbert", "Frederick", "Ray", "Joel", "Edwin", "Don", "Eddie", "Ricky", "Troy", "Randall", "Barry", "Alexander", "Bernard", "Mario", "Leroy", "Francisco", "Marcus", "Micheal", "Theodore", "Clifford", "Miguel", "Oscar", "Jay", "Jim", "Tom", "Calvin", "Alex", "Jon", "Ronnie", "Bill", "Lloyd", "Tommy", "Leon", "Derek", "Warren", "Darrell", "Jerome", "Floyd", "Leo", "Alvin", "Tim", "Wesley", "Gordon", "Dean", "Greg", "Jorge", "Dustin", "Pedro", "Derrick", "Dan", "Lewis", "Zachary", "Corey", "Herman", "Maurice", "Vernon", "Roberto", "Clyde", "Glen", "Hector", "Shane", "Ricardo", "Sam", "Rick", "Lester", "Brent", "Ramon", "Charlie", "Tyler", "Gilbert", "Gene", "Marc", "Reginald", "Ruben", "Brett", "Angel", "Nathaniel", "Rafael", "Leslie", "Edgar", "Milton", "Raul", "Ben", "Chester", "Cecil", "Duane", "Franklin", "Andre", "Elmer", "Brad", "Gabriel", "Ron", "Mitchell", "Roland", "Arnold", "Harvey", "Jared", "Adrian", "Karl", "Cory", "Claude", "Erik", "Darryl", "Jamie", "Neil", "Jessie", "Christian", "Javier", "Fernando", "Clinton", "Ted", "Mathew", "Tyrone", "Darren", "Lonnie", "Lance", "Cody", "Julio", "Kelly", "Kurt", "Allan", "Nelson", "Guy", "Clayton", "Hugh", "Max", "Dwayne", "Dwight", "Armando", "Felix", "Jimmie", "Everett", "Jordan", "Ian", "Wallace", "Ken", "Bob", "Jaime", "Casey", "Alfredo", "Alberto", "Dave", "Ivan", "Johnnie", "Sidney", "Byron", "Julian", "Isaac", "Morris", "Clifton", "Willard", "Daryl", "Ross", "Virgil", "Andy", "Marshall", "Salvador", "Perry", "Kirk", "Sergio", "Marion", "Tracy", "Seth", "Kent", "Terrance", "Rene", "Eduardo", "Terrence", "Enrique", "Freddie", "Wade", "Mary", "Patricia", "Linda", "Barbara", "Elizabeth", "Jennifer", "Maria", "Susan", "Margaret", "Dorothy", "Lisa", "Nancy", "Karen", "Betty", "Helen", "Sandra", "Donna", "Carol", "Ruth", "Sharon", "Michelle", "Laura", "Sarah", "Kimberly", "Deborah", "Jessica", "Shirley", "Cynthia", "Angela", "Melissa", "Brenda", "Amy", "Anna", "Rebecca", "Virginia", "Kathleen", "Pamela", "Martha", "Debra", "Amanda", "Stephanie", "Carolyn", "Christine", "Marie", "Janet", "Catherine", "Frances", "Ann", "Joyce", "Diane", "Alice", "Julie", "Heather", "Teresa", "Doris", "Gloria", "Evelyn", "Jean", "Cheryl", "Mildred", "Katherine", "Joan", "Ashley", "Judith", "Rose", "Janice", "Kelly", "Nicole", "Judy", "Christina", "Kathy", "Theresa", "Beverly", "Denise", "Tammy", "Irene", "Jane", "Lori", "Rachel", "Marilyn", "Andrea", "Kathryn", "Louise", "Sara", "Anne", "Jacqueline", "Wanda", "Bonnie", "Julia", "Ruby", "Lois", "Tina", "Phyllis", "Norma", "Paula", "Diana", "Annie", "Lillian", "Emily", "Robin", "Peggy", "Crystal", "Gladys", "Rita", "Dawn", "Connie", "Florence", "Tracy", "Edna", "Tiffany", "Carmen", "Rosa", "Cindy", "Grace", "Wendy", "Victoria", "Edith", "Kim", "Sherry", "Sylvia", "Josephine", "Thelma", "Shannon", "Sheila", "Ethel", "Ellen", "Elaine", "Marjorie", "Carrie", "Charlotte", "Monica", "Esther", "Pauline", "Emma", "Juanita", "Anita", "Rhonda", "Hazel", "Amber", "Eva", "Debbie", "April", "Leslie", "Clara", "Lucille", "Jamie", "Joanne", "Eleanor", "Valerie", "Danielle", "Megan", "Alicia", "Suzanne", "Michele", "Gail", "Bertha", "Darlene", "Veronica", "Jill", "Erin", "Geraldine", "Lauren", "Cathy", "Joann", "Lorraine", "Lynn", "Sally", "Regina", "Erica", "Beatrice", "Dolores", "Bernice", "Audrey", "Yvonne", "Annette", "June", "Samantha", "Marion", "Dana", "Stacy", "Ana", "Renee", "Ida", "Vivian", "Roberta", "Holly", "Brittany", "Melanie", "Loretta", "Yolanda", "Jeanette", "Laurie", "Katie", "Kristen", "Vanessa", "Alma", "Sue", "Elsie", "Beth", "Jeanne", "Vicki", "Carla", "Tara", "Rosemary", "Eileen", "Terri", "Gertrude", "Lucy", "Tonya", "Ella", "Stacey", "Wilma", "Gina", "Kristin", "Jessie", "Natalie", "Agnes", "Vera", "Willie", "Charlene", "Bessie", "Delores", "Melinda", "Pearl", "Arlene", "Maureen", "Colleen", "Allison", "Tamara", "Joy", "Georgia", "Constance", "Lillie", "Claudia", "Jackie", "Marcia", "Tanya", "Nellie", "Minnie", "Marlene", "Heidi", "Glenda", "Lydia", "Viola", "Courtney", "Marian", "Stella", "Caroline", "Dora", "Jo", "Vickie", "Mattie"]

lastNames :: [Text]
lastNames =
  ["Smith", "Johnson", "Williams", "Jones", "Brown", "Davis", "Miller", "Wilson", "Moore", "Taylor", "Anderson", "Thomas", "Jackson", "White", "Harris", "Martin", "Thompson", "Garcia", "Martinez", "Robinson", "Clark", "Rodriguez", "Lewis", "Lee", "Walker", "Hall", "Allen", "Young", "Hernandez", "King", "Wright", "Lopez", "Hill", "Scott", "Green", "Adams", "Baker", "Gonzalez", "Nelson", "Carter", "Mitchell", "Perez", "Roberts", "Turner", "Phillips", "Campbell", "Parker", "Evans", "Edwards", "Collins", "Stewart", "Sanchez", "Morris", "Rogers", "Reed", "Cook", "Morgan", "Bell", "Murphy", "Bailey", "Rivera", "Cooper", "Richardson", "Cox", "Howard", "Ward", "Torres", "Peterson", "Gray", "Ramirez", "James", "Watson", "Brooks", "Sanders", "Price", "Bennett", "Wood", "Barnes", "Ross", "Henderson", "Coleman", "Jenkins", "Perry", "Powell", "Long", "Patterson", "Hughes", "Flores", "Washington", "Butler", "Simmons", "Foster", "Gonzales", "Bryant", "Alexander", "Russell", "Griffin", "Diaz", "Hayes"]
