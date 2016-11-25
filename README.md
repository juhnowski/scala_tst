## Verbose testing

By default, tests print no information. However, it is possible to make them verbose (to see generated Scala code):


    $ sbt
    > eval System.setProperty("test.verbose", "true")
    > test
