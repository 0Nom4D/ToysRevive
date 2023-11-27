module TestServer (testSuite) where

import API.ServerMessage
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

testSuite :: Test
testSuite =
    testGroup
        "Server's Test"
        [ testCase "Hello World" $ messageEndpoint @=? ServerMessage "Hello World"
        ]
