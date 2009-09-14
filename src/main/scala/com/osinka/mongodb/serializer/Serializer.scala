package com.osinka.mongodb.serializer

import com.mongodb._
import com.osinka.mongodb._

trait Serializer[T] {
    def in(x: T): Option[DBObject]
    def out(dbo: DBObject): Option[T]
}

object PlainDBOSerializer extends Serializer[DBObject] {
    def in(x: DBObject) = {
        assert(x != null)
        Some(x)
    }

    def out(dbo: DBObject) = {
        assert(dbo != null)
        Some(dbo)
    }
}

class ElementSerializer[T](val element: CollectionElement[T]) extends Serializer[T] {
    def in(x: T): Option[DBObject] = {
        assert(false, "ElementSerializer not implemented")
        None
    }

    def out(dbo: DBObject): Option[T] = {
        assert(false, "ElementSerializer not implemented")
        None
    }
}

/*
import java.beans._
import java.lang.reflect._
import scala.collection._
import scala.reflect.Manifest
import com.mongodb.{DBObject,BasicDBObject}

trait Serializer {
    def provide[T <: AnyRef](obj: T): Option[DBObject]

    def expect[T <: AnyRef](dbo: DBObject)(implicit m: Manifest[T]): T
    
    def merge[T <: AnyRef](obj: T, dbO: DBObject): T
}

object Serializer extends Serializer {
    private lazy val ignoreProps = mutable.HashSet("class")

    def primitive_?(clazz: Class[_]) =
        clazz.isPrimitive ||
        classOf[Number].isAssignableFrom(clazz) ||
        clazz.equals(classOf[Boolean]) ||
        clazz.equals(classOf[java.lang.Boolean])

    def encodeField[T](name: String, obj: T): Option[(String, AnyRef)] = tryo(obj).flatMap{oo => oo match {
        case v: AnyVal => Some( (name, v.toString) )
        case o: Some[_] => encodeField(name, o.get)
        case None => None
        case m: Map[_,_] =>
            val dbo = new BasicDBObject
            for {val (k,v) <- m
                 val (name, value) <- encodeField(k.toString, v)}
            dbo.append(name, value)
            Some( (name, dbo) )
        case v: AnyRef if primitive_?(niceClass(v)) => Some( (name, v.toString) )
        case v: AnyRef =>
            val clazz = niceClass(v)
            val dbo = new BasicDBObject
            for{val property <- Introspector.getBeanInfo(clazz).getPropertyDescriptors if !ignoreProps.contains(property.getName)
                val method = property.getReadMethod
                val (name, value) <- encodeField(property.getName, method.invoke(obj, null))}

                dbo.append(property.getName, value)
            Some( (name, dbo) )
    } }

    /**
     *
     def applyPfs(pf: PartialFunction[T,R])(obj: T): Option[R]

     eField = primitives orElse option orElse map orElse asString
     eToplevel = notString orElse map orElse obj
     */
    override def provide[T <: AnyRef](obj: T): Option[DBObject] = tryo(obj).flatMap{oo => oo match {
        case s: String => None
        case m: Map[_,_] =>
            val dbo = new BasicDBObject
            for {val (k,v) <- m
                 val (name, value) <- encodeField(k.toString, v)}

                dbo.append(name, value)
            Some(dbo)
        case _ =>
            val clazz = niceClass(obj)
            val dbo = new BasicDBObject
            for{val property <- Introspector.getBeanInfo(clazz).getPropertyDescriptors if !ignoreProps.contains(property.getName)
                val method = property.getReadMethod
                val (name, value) <- encodeField(property.getName, method.invoke(obj, null))}

                dbo.append(property.getName, value)
            Some(dbo)
    } }

    override def expect[T <: AnyRef](dbo: DBObject)(implicit m: Manifest[T]): T = m.erasure.newInstance.asInstanceOf[T]

    override def merge[T <: AnyRef](obj: T, dbO: DBObject): T = throw new Exception
}
*/