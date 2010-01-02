package com.osinka.mongodb.shape

import org.specs._
import org.specs.runner._

class shapesTest extends JUnit4(shapesSpec) with Console
object shapesTestRunner extends ConsoleRunner(shapesSpec)

object shapesSpec extends Specification {
    "Shapes".areSpecifiedBy(serializerSpec, fieldsSpec, collectionSpec, querySpec)
}