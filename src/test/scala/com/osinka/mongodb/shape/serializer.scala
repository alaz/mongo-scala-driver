package com.osinka.mongodb.shape

import org.specs._
import java.util.Date
import com.mongodb.{DBObject, BasicDBObjectBuilder}

import Preamble._
import wrapper.DBO

object serializerSpec extends Specification {
    val Const = "John Doe"
    val jd = DBO.fromMap( Map("name" -> Const) )

    "Shape scalars" should {
        object IntS extends TSerializer[Int]( () => Holder[Int](99) )
        object StringS extends TSerializer[String]( () => Holder[String]("init") )
        object DateS extends TSerializer[Date]( () => Holder[Date](new Date))

        "serialize AnyVals" in {
            IntS.i.valueOf(Holder[Int](1)) must be_==( Some(1) )

            val h = Holder[Int](10)
            IntS.i(h) = 1
            h.value must be_==(1)
        }
        "serialize Ints" in {
            BasicDBObjectBuilder.start("i", 1).get must beLike {
                case IntS(o) => o.value == 1
            }

            val dbo2 = IntS( Holder[Int](1) )
            dbo2.get("i") must (notBeNull and be_==(1))
        }
        "serialize Strings" in {
            BasicDBObjectBuilder.start("i", "test").get must beLike {
                case StringS(o) => o.value == "test"
            }

            val dbo2 = StringS( Holder[String]("test") )
            dbo2.get("i") must (notBeNull and be_==("test"))
        }
        "serialize Dates" in {
            BasicDBObjectBuilder.start("i", new Date(1)).get must beLike {
                case DateS(o) => o.value == new Date(1)
            }

            val dbo2 = DateS( Holder[Date](new Date(1)) )
            dbo2.get("i") must (notBeNull and be_==(new Date(1)))
        }
        "serialize Maps" in {
            skip("not implemented")
        }
        "serialize Arrays" in {
            skip("not implemented")
        }
    }
    "Case class Shape" should {
         val jd = DBO.fromMap( Map("name" -> Const) )

        "serialize to DBObject" in {
            val dbo = CaseUser in new CaseUser(Const)
            dbo must notBeNull
            dbo.get("name") must be_==(Const)
        }
        "serialize from DBObject" in {
            CaseUser.out(jd) must beSome[CaseUser].which{_.name == Const}
        }
        "not include _id and _ns into DBO" in {
            val shape = CaseUser.constraints
            shape must haveSuperClass[Map[String, Map[String,Boolean]]]
            shape.get("name") must beSome[Map[String,Boolean]].which{_.get("$exists") == Some(true)}
            shape.get("_id") must beNone
            shape.get("_ns") must beNone
        }
        "mirror mongo fields back to object" in {
            import com.mongodb.ObjectId

            val dbo = DBO.empty
            dbo.putAll(jd)

            val u = CaseUser out dbo
            u must beSome[CaseUser]
            u.get.name must be_==(Const)
            u.get.mongoOID must beNull

            dbo.put("_id", ObjectId.get)
            CaseUser.mirror(u.get)(dbo)
            u.get.mongoOID must be_==(dbo.get("_id"))
        }
    }
    "Ordinary class Shape" should {
        "serialize to DBObject" in {
            val u = new OrdUser
            u.name = Const
            val dbo = OrdUser in u
            dbo.get("name") must be_==(Const)
        }
        "deserialize from DBObject" in {
            OrdUser.out(jd) must beSome[OrdUser].which{_.name == Const}
        }
    }
    "Class with Embedded object Shape" should {
        "serialize  to DBObject" in {
            val dbo = ComplexType in new ComplexType(CaseUser(Const), 1)
            dbo.get("user") must haveSuperClass[DBObject]
            dbo.get("user").asInstanceOf[DBObject].get("name") must be_==(Const)
            dbo.get("msgs") must haveClass[java.lang.Integer]
            dbo.get("msgs") must be_==(1)
        }
        "deserialize from DBObject" in {
            DBO.fromMap( Map("user" -> jd, "msgs" -> 1) ) match {
                case ComplexType(c) =>
                    c.user must notBeNull
                    c.user.name must be_==(Const)
                    c.messageCount must (notBeNull and be_==(1))
                case _ =>
                    fail("had to extract ComplexType out from DBO")
            }
        }
    }
    "Optional field" should {
        "have empty constraints" in {
            OptModel.description.constraints must beEmpty
            OptModel.description2.constraints must beEmpty
            OptModel.description3.constraints must beEmpty
            OptModel.comment.constraints must beEmpty
        }
        "serialize to DBObject" in {
            skip("todo")
        }
        "deserialize from DBObject" in {
            skip("todo")
        }
    }
}