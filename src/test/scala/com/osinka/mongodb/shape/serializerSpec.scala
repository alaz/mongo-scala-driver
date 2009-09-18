package com.osinka.mongodb.shape

import org.specs._
import org.specs.runner._
import com.mongodb.{DBObject, BasicDBObjectBuilder}

class serializerTest extends JUnit4(serializerSpec) with Console
object serializerTestRunner extends ConsoleRunner(serializerSpec)

object serializerSpec extends Specification {
    "Shape serializer" should {
        "translate object to DBObject / case" in {
            val dbo = BasicDBObjectBuilder.start.get
            CaseUser(dbo) = CaseUser("John Doe")
            dbo.get("name") must be_==("John Doe")
        }
        "translate object to DBObject / ord" in {
            val dbo = BasicDBObjectBuilder.start.get
            val u = new OrdUser
            u.name = "John Doe"
            OrdUser(dbo) = u
            dbo.get("name") must be_==("John Doe")
        }
        "translate DBObject to object / case" in {
            skip("not implemented")
        }
        "translate DBObject to object / ord" in {
            skip("not implemented")
        }
        "skip readonly fields on write" in {
            skip("not implemented")
        }
        "skip writeonly fields on read" in {
            skip("not implemented")
        }
    }
}