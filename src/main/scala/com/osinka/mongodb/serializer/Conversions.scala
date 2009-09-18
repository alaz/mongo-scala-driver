package com.osinka.mongodb.serializer

import com.mongodb._

trait Conversions {

    /**
     * It probably would be more effective to implement mapToDBO via
     * BasicDBObject.putAll(java.util.Map) or BasicDBObjectBuilder.start(j.u.Map)
     * if they supported nested complex structures. But they assume all the Map
     * values are scalars (DBObjects or Strings or Integers, etc.)
     */
    def createDBObject(m: Map[String, Any]): DBObject = {
        def wrap(obj: Any): Option[Any] = obj match {
            case m: Map[_, _] =>
                // to avoid type erasure warning
                Some( createDBObject(m.asInstanceOf[Map[String, Any]]) )
            case iterable: Iterable[_] =>
                val ret = new BasicDBList
                for {(v, i) <- iterable.toList.zipWithIndex}
                    wrap(v).map{ret.put(i, _)}
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

    def mirrorMeta(obj: DBObject): Map[String, String] = {
        import scala.collection.immutable.Map
        import com.osinka.mongodb.Helper._

        val keys = "_id" :: "_ns" :: Nil
        val l = for {val key <- keys
                     val value <- tryo(obj.get(key))}
                yield key -> value.toString

        Map.empty ++ l
    }

    implicit def mapToDBObject(m: Map[String, Any]): DBObject = createDBObject(m)
}
