library('RUnit')

test.suite = defineTestSuite("example",
                             dirs = file.path("test/testOptiMoveEntirePort"),
                             testFileRegexp = '.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)