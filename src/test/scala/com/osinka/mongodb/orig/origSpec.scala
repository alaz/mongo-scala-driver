package com.osinka.mongodb.orig

import org.specs._
import org.specs.runner._

class origTest extends JUnit4(origSpec) with Console
object origTestRunner extends ConsoleRunner(origSpec)

object origSpec extends Specification {
    "original Java API extra".isSpecifiedBy( plainSpec )
}
