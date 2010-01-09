/**
 * Copyright (C) 2009-2010 Alexander Azarov <azarov@osinka.ru>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
        "have constraint" in {
            ComplexType.user.mongoFieldName must be_==("user")
            ComplexType.user.containerPath must haveTheSameElementsAs(List("user"))
            ComplexType.constraints must havePair("user.name" -> Map("$exists" -> true))
        }
        "have proper shape for embedded object" in {
            val nameField = ComplexType.user.name
            nameField must haveSuperClass[ObjectField[ComplexType]]
            nameField.mongoConstraints.get("user.name") must beSome[Map[String,Boolean]].which{_.get("$exists") == Some(true)}
        }
    }
    "Ref field" should {
        object RefModel extends RefModelShape(mongo, "users")
        "have constraint" in {
            RefModel.user.mongoFieldName must be_==("user")
            RefModel.user.mongoFieldPath must haveTheSameElementsAs(List("user"))
            RefModel.constraints must havePair("user" -> Map("$exists" -> true))
        }
    }
    "ArrayOfInt field" should {
        import ArrayOfInt._
        "have constraint" in {
            ArrayModel.messages.mongoFieldName must be_==("messages")
            ArrayModel.constraints must havePair("messages" -> Map("$exists" -> true))
        }
    }
    "ArrayOfEmbedded field" should {
        import ArrayOfEmbedded._
        "have constraint" in {
            // we cannot ask for "users.name" because the array can be empty
            ArrayModel.constraints must notHaveKey("users.name")
            ArrayModel.constraints must havePair("users" -> Map("$exists" -> true))
        }
    }
    "ArrayOfRef field" should {
        import ArrayOfRef._
        object ArrayModel extends ArrayModelShape(mongo, "users")
        "have constraint" in {
            ArrayModel.constraints must havePair("users" -> Map("$exists" -> true))
        }
    }
}