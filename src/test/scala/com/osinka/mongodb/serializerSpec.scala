package com.osinka.mongodb

import org.specs._
import org.specs.runner._

class serializerTest extends JUnit4(serializerSpec) with Console
object serializerTestRunner extends ConsoleRunner(serializerSpec)

object serializerSpec extends Specification {
    "Serializer" should {
        "encode null" in {
            true must beTrue
        }
    }
}