library('RUnit')

test.suite = defineTestSuite("example",
                             dirs = file.path("test/testOptiMoveOneTradeEachTime"),
                             testFileRegexp = '.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)