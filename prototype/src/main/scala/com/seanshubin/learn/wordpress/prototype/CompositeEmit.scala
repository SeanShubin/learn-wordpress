package com.seanshubin.learn.wordpress.prototype

class CompositeEmit(parts:String => Unit*) extends (String => Unit) {
  override def apply(line: String): Unit = parts.foreach(_.apply(line))
}
