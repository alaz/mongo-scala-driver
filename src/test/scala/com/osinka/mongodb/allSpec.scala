package com.osinka.mongodb

import org.specs._
import org.specs.runner._

class allTest extends JUnit4(allSpec) with Console
object allTestRunner extends ConsoleRunner(allSpec)

object allSpec extends Specification {
   "mongo-scala-driver".isSpecifiedBy(conversionsSpec, collectionSpec, querySpec, shapesSpec)
}

object shapesSpec extends Specification {
    import shape._

    "Shapes".areSpecifiedBy(serializerSpec, shapeSpec, querySpec)
}