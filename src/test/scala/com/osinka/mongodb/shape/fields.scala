package com.osinka.mongodb.shape

import org.specs._
import com.mongodb._

import Preamble._
import Config._

object fieldsSpec extends Specification("Shape fields") {
    val CollName = "test"
    val Const = "John Doe"

    val mongo = new Mongo(Host, Port).getDB(Database)

    doAfter { mongo.dropDatabase }

    "Case Class" should {
        "declare fields" in {
            CaseUser.fieldList must haveSize(3)
            CaseUser.fieldList must contain(CaseUser.name)
        }
        "have proper parentFields" in {
            CaseUser.containerPath must beEmpty
            CaseUser.name.mongoFieldPath must haveTheSameElementsAs("name" :: Nil)
        }
    }
    "Class Shape" should {
        "declare fields" in {
            OrdUser.fieldList must haveSize(3)
            OrdUser.fieldList must contain(OrdUser.name)
        }
    }
    "Complex Shape" should {
        "declare fields" in {
            ComplexType.user must notBeNull
            ComplexType.fieldList must haveSize(4)
            ComplexType.fieldList must contain(ComplexType.user)
        }
        "have proper parentFields" in {
            ComplexType.containerPath must beEmpty
            ComplexType.user.containerPath must haveTheSameElementsAs("user" :: Nil)
            ComplexType.user.name.mongoFieldPath must haveTheSameElementsAs("name" :: "user" :: Nil)
        }
        "have proper shape for embedded object" in {
            val nameField = ComplexType.user.name
            nameField must haveSuperClass[ObjectField[ComplexType]]
            nameField.mongoConstraints.get("user.name") must beSome[Map[String,Boolean]].which{_.get("$exists") == Some(true)}
        }
    }
}