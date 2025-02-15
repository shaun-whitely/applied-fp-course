{-# LANGUAGE OverloadedStrings #-}
module Main where

-- | **REMINDER**
-- This level is not an isolated module to complete. This level exists as one
-- starting module: `test/Test.hs`. Which you are to import your most recently
-- completed `Application` to be tested.
--
-- As you progress through the course, you are encouraged to return to this
-- `test/Test.hs` and update it so you're able to be confident that your
-- application will behave as you expect. You may also write your tests before
-- you write your functions, this can be useful when trying to think through a
-- problem.

-- | This is the only location for tests as you progress through the course.

-- | This module starts our very sparse. There are only the imports required to
-- have these initial tests work. Additional tests are for you to build. and may
-- require you to import other modules as you require.
--
-- As you progress through the levels, the initialisation of the 'app' will
-- become more complicated as more components are introduced. Part of the
-- exercise is to work out how to integrate your application with this testing
-- framework.

-- | 'tasty' takes care of managing all of our test cases, running them,
-- checking results and then providing us with a report.
import           Test.Tasty         (defaultMain, testGroup)
import           Test.Tasty.HUnit   (assertEqual)

-- | 'tasty-wai' makes it easier to create requests to submit to our
-- application, and provides some helper functions for checking our assertions.
import           Test.Tasty.Wai     (assertBody, assertBodyContains,
                                     assertStatus', get, post,
                                     testWai)

-- | For running unit tests for individual functions, we have included the
-- 'tasty-hunit' package. More information is available on the Hackage page:
-- https://hackage.haskell.org/package/tasty-hunit.
--
-- import qualified Test.Tasty.HUnit as HU
--

import           Network.HTTP.Types as HTTP
import           Network.Wai        (Application)
import qualified Network.Wai        as Wai

-- | This import is provided for you so you can check your work from Level02. As
-- you move forward, come back and import your latest 'Application' so that you
-- can test your work as you progress.
import           Level07.AppM       (Env (..))
import qualified Level07.Core       as Core
import qualified Level07.DB         as DB
import           Level07.Types      (Conf(..), Topic(..), CommentText(..), DBFilePath(..),
                                     Port(..))

initTestDb :: IO DB.FirstAppDB
initTestDb = do
  errorOrDb <- DB.initDB (DBFilePath ":memory:")
  either (\_ -> fail "Could not initialise in-memory DB") (pure . id) errorOrDb

testConf :: Conf
testConf = Conf (Port 3000) (DBFilePath ":memory:")

testEnv :: DB.FirstAppDB -> Env
testEnv db = Env
  { envLoggingFn = const $ pure ()
  , envConfig = testConf
  , envDB = db
  }

main :: IO ()
main = do
  db <- initTestDb
  let env = testEnv db
  let app = Core.app env
  defaultMain $ testGroup "Applied FP Course - Tests"
    [ testGroup "/list"
      [ testWai app "lists topics" $ do
        post "fudge/add" "is delicious"
        post "cheese/add" "is tasty"
        resp <- get "list"
        assertStatus' HTTP.status200 resp
        assertBodyContains "fudge" resp
        assertBodyContains "cheese" resp
      ]

    , testGroup "/add"
      [ testWai app "with empty input reports error" $ do
        resp <- post "fudge/add" ""
        assertStatus' HTTP.status400 resp
        assertBody "Empty Comment" resp
      ]
    ]
