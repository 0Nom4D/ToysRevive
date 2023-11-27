import Test.Framework (defaultMain)
import TestServer

main :: IO ()
main = do
    defaultMain
        [ TestServer.testSuite
        ]
