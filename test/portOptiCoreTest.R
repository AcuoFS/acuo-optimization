library('RUnit')

test.suite = defineTestSuite("example",
                             dirs = file.path("test/testPortOptiCore"),
                             testFileRegexp = '.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)