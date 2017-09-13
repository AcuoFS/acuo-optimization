library('RUnit')

test.suite = defineTestSuite("example",
                             dirs = file.path("test/testPortOptiMoveEntirePortMain"),
                             testFileRegexp = '.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)