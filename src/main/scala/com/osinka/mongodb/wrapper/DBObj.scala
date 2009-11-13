package com.osinka.mongodb.wrapper

import com.mongodb.{DBObject, BasicDBObject}

object DBO {
    def empty = new BasicDBObject

    def fromMap(m: Map[String,Any]): DBObject = {
        import com.mongodb.{BasicDBObjectBuilder, BasicDBList}

        def wrap(obj: Any): Option[Any] = obj match {
            case m: Map[_, _] =>
                // to avoid type erasure warning
                Some( fromMap(m.asInstanceOf[Map[String, Any]]) )
            case iterable: Iterable[_] =>
                val ret = new BasicDBList
                for {val (v, i) <- iterable.toList.zipWithIndex
                     val wrapped <- wrap(v)}
                    ret.put(i, wrapped)
                Some(ret)
//            case ref: Ref[_] =>
            case None => None
            case Some(v) => wrap(v)
            case _ => Some(obj)
        }

        def acc(dbo: BasicDBObjectBuilder, leaf: (String, Any)): BasicDBObjectBuilder =
            wrap(leaf._2).map{dbo.append(leaf._1, _)} getOrElse dbo

        (m foldLeft BasicDBObjectBuilder.start)(acc(_, _)).get
    }

    def merge(dbo1: DBObject, dbo2: DBObject) = {
        val dbo = empty
        dbo putAll dbo1
        dbo putAll dbo2
        dbo
    }

    def mirrorMeta(obj: DBObject): Map[String, String] = {
        import scala.collection.immutable.Map
        import Preamble.tryo

        val keys = "_id" :: "_ns" :: Nil
        val l = for {val key <- keys
                     val value <- tryo(obj.get(key))}
                yield key -> value.toString

        Map.empty ++ l
    }
}
