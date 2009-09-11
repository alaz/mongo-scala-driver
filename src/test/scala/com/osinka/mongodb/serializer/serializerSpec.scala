package com.osinka.mongodb.serializer

import org.specs._
import org.specs.runner._

class serializerTest extends JUnit4(serializerSpec) with Console
object serializerTestRunner extends ConsoleRunner(serializerSpec)

object serializerSpec extends Specification {
    "Serializer" should {
        "encode null" in {
            skip("TODO serializer spec")
        }
    }
}