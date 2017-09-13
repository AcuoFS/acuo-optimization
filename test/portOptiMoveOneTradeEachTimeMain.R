library('RUnit')

test.suite = defineTestSuite("example",
                             dirs = file.path("test/testPortOptiMoveOneTradeEachTimeMain"),
                             testFileRegexp = '.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)