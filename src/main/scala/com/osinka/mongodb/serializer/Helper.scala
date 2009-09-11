package com.osinka.mongodb

trait Helper {
    private[mongodb] def niceClass[T <: AnyRef](x: T): Class[_ <: T] =
        x.getClass.asInstanceOf[Class[T]]

    private[mongodb] def tryo[T](obj: T): Option[T] =
        if (null == obj) None
        else Some(obj)
}
