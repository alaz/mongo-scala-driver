package com.osinka.mongodb

import org.specs._
import org.specs.runner._

class baseTest extends JUnit4(baseSpec) with Console
object baseTestRunner extends ConsoleRunner(baseSpec)

object baseSpec extends Specification {
   "Base".isSpecifiedBy(conversionsSpec, collectionSpec, querySpec)
}