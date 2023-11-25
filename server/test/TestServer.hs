module TestServer (testSuite) where

import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?))
import API.ServerMessage
import Test.Framework.Providers.HUnit (testCase)

testSuite :: Test
testSuite = testGroup "Server's Test" [
        testCase "Hello World" $ messageEndpoint @=? (ServerMessage "Hello World")
    ]
