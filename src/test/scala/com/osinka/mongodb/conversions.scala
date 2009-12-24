package com.osinka.mongodb

import org.specs._
import com.mongodb._

import com.osinka.mongodb.Config._

object conversionsSpec extends Specification("Implicit conversions") {
    import wrapper.DBO

    "Map to DBObject" should {
        "convert Any values" in {
            DBO.fromMap(Map("a" -> 1)) must (containField("a") and verify(_.get("a") == 1))
            DBO.fromMap(Map("a" -> 2.0)) must (containField("a") and verify(_.get("a") == 2.0))
            DBO.fromMap(Map("a" -> "str")) must (containField("a") and verify(_.get("a") == "str"))
        }
        "convert Option" in {
            DBO.fromMap(Map("a" -> None)) must not(containField("a"))
            DBO.fromMap(Map("a" -> Some("b"))) must (containField("a") and verify(_.get("a") == "b"))
        }
        "convert Lists" in {
            listExample(DBO.fromMap(Map("a" -> List("a", "b"))))
        }
        "convert Map of Lists" in {
            val dbo = DBO.fromMap(Map("complex" -> Map("a" -> ("a" :: "b" :: Nil) )))
            dbo must containField("complex")
            dbo.get("complex") must haveSuperClass[DBObject]
            listExample(dbo.get("complex").asInstanceOf[DBObject])
        }
        "convert List of Options" in {
            val list = List(Some(1), None, Some(2), None, Some(3))
            val dbo = DBO.fromMap(Map("a" -> list))
            dbo must containField("a")
            dbo.get("a") must haveSuperClass[DBObject]

            val ldbo = dbo.get("a").asInstanceOf[DBObject]
            ldbo must (containField("0") and verify(_.get("0") == 1))
            ldbo must (containField("1") and verify(_.get("1") == null))
            ldbo must (containField("2") and verify(_.get("2") == 2))

            DBO.toArray(ldbo).map{Preamble.tryo[Any]} must haveTheSameElementsAs(list)
        }
        "convert Map of Maps" in {
            val dbo = DBO.fromMap(Map("a" -> Map("b" -> "value")))
            dbo must containField("a")
            dbo.get("a") must haveSuperClass[DBObject]
            dbo.get("a").asInstanceOf[DBObject] must (containField("b") and verify{_.get("b") == "value"})
        }
    }

    def listExample(dbo: DBObject) {
        dbo must containField("a")
        dbo.get("a") must haveSuperClass[DBObject]
        dbo.get("a").asInstanceOf[DBObject] must (containField("0") and verify(_.get("0") == "a"))
        dbo.get("a").asInstanceOf[DBObject] must (containField("1") and verify(_.get("1") == "b"))
    }
}

import org.specs.matcher.Matcher
case class containField(name: String) extends Matcher[DBObject] {
    def apply(dbo: => DBObject) =
        (dbo.containsField(name), "Contains "+name, "does not contain "+name)
}
