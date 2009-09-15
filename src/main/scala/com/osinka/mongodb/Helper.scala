package com.osinka.mongodb

object Helper {
    private[mongodb] def niceClass[T <: AnyRef](x: T): Class[_ <: T] =
        x.getClass.asInstanceOf[Class[T]]

    private[mongodb] def tryo[T](obj: T): Option[T] =
        if (null == obj) None
        else Some(obj)
        
    private[mongodb] def pfToOption[A, B](f: PartialFunction[A,B])(a: A) =
        if (f.isDefinedAt(a)) Some(f(a))
        else None
}