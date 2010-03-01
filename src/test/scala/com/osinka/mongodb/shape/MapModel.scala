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

import com.mongodb.{DB,DBObject}
import com.osinka.mongodb._

object MapOfScalar {
    class MapModel(val id: Int) {
        var counts: Map[String, Int] = Map.empty

        override def toString = "ArrayModel("+id+", "+counts.mkString("[",",","]")+")"
    }

    object MapModel extends ObjectShape[MapModel] { shape =>
        lazy val id = Field.scalar("id", _.id)

        lazy val counts = Field.map("counts", _.counts, (x: MapModel, l: Map[String,Int]) => x.counts = l)
//
//   same as
//
//        object messages extends MongoMap[Int] with ScalarContent[Int] {
//            override val mongoFieldName = "count"
//            override val rep = Represented.by[Map[String,Int]](_.count, Some( (x: MapModel, l: Map[String,Int]) => x.count = l ))
//        }

        lazy val * = List(id, counts)
        override def factory(dbo: DBObject) = for {_id <- id from dbo} yield new MapModel(_id)
    }
}

object MapOfEmbedded {
    class MapModel(val id: Int, val users: Map[String,CaseUser]) {
        override def toString = "MapModel("+id+","+users+")"
    }

    object MapModel extends ObjectShape[MapModel] { shape =>
        lazy val id = Field.scalar("id", _.id)

        object users extends MapEmbeddedField[CaseUser]("users", _.users, None) with CaseUserIn[MapModel] { field =>
            def apply(key: String) = new shape.EmbeddedField[CaseUser](key, _.users(key), None) with CaseUserIn[MapModel] {
                override def mongoFieldPath = field.mongoFieldPath ::: super.mongoFieldPath
            }
        }
//
//   same as
//
//        object users extends MongoMap[CaseUser] with EmbeddedContent[CaseUser] with CaseUserIn[MapModel] {
//            override val mongoFieldName = "users"
//            override val rep = shape.Represented.by[Map[String, CaseUser]]( _.users, None)
//        }

        lazy val * = List(id, users)
        override def factory(dbo: DBObject) = for {_id <- id from dbo; _users <- users from dbo} yield new MapModel(_id, _users)
    }
}