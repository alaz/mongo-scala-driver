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

package com.osinka.mongodb.orig

import org.specs._
import org.specs.runner._
import com.mongodb._

import com.osinka.mongodb.Config._

// FIXME: how to make mongodb reflection working??!

//class reflectionTest extends JUnit4(reflectionSpec) with Console
//object reflectionTestRunner extends ConsoleRunner(reflectionSpec)

object reflectionSpec extends Specification("ReflectionDBObject Spec") {
    val mongo = new Mongo(Host, Port).getDB(Database)

    doAfter { mongo.dropDatabase }

    "Collection" should {
        val coll = mongo.getCollection("test")

        doBefore { mongo.requestStart }
        doAfter  { mongo.requestDone; coll.drop }

        "have initial zero size" in {
            coll.getCount must be_==(0)
        }
        "support case classes" in {
            case class TestClass(var i: Int) extends ReflectionDBObject {
                def this() = this(-1)
            }
            coll.setObjectClass(classOf[TestClass])

            val insval = TestClass(1)
            coll insert insval
            insval must haveClass[TestClass]
            insval must beLike {
                case t @ TestClass(i) =>
                    t.get_id must notBeNull
                    //ObjectId.isValid()
            }
            
            val retval = coll.findOne
            retval must haveClass[TestClass]
            retval must be_==(TestClass(1))
        }
        "support ordinary classes" in {
            class TestClass(var i: Int) extends ReflectionDBObject {
                def this() = this(-1)

                override def equals(other: Any): Boolean = other match {
                    case o: TestClass => o.i == i
                    case _ => false
                }
            }
            coll.setObjectClass(classOf[TestClass])

            val insval = new TestClass(1)
            coll insert insval
            insval must haveClass[TestClass]
            insval must beLike {
                case t: TestClass =>
                    t.get_id must notBeNull
                    //ObjectId.isValid()
            }

            val retval = coll.findOne
            retval must haveClass[TestClass]
            retval must be_==(new TestClass(1))
        }
        "support top-level ord classes" in {
            coll.setObjectClass(classOf[Class2])

            coll.getCount must be_==(0)

            val insval = (new Class2).set(1)
            coll.save(insval)
            insval must haveClass[Class2]
            insval must beLike {
                case t: Class2 =>
                    t.get_id aka "ObjectId" must notBeNull
                    t.value aka "Inserted object value" must be_==(1)
                    //ObjectId.isValid()
            }

            val retval = coll.findOne
            retval must haveClass[Class2]
            retval must beLike {
                case c: Class2 =>
                    c.get_id must be_==(insval.asInstanceOf[Class2].get_id)
                    c.value aka "Requested object value" must be_==(1)
            }
        }
    }
}

import scala.reflect.BeanProperty
@BeanProperty
class Class2 extends ReflectionDBObject {
    var value: Int = _

    def set(n: Int): this.type = {
        value = n
        this
    }
}
