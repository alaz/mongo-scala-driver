package com.osinka.mongodb

private[mongodb] object Helper {
    def niceClass[T <: AnyRef](x: T): Class[_ <: T] = x.getClass.asInstanceOf[Class[T]]

    def tryo[T](obj: T): Option[T] =
        if (null == obj) None
        else Some(obj)
}
