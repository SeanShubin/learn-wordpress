package com.seanshubin.learn.wordpress.prototype

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import scala.collection.JavaConverters._

class PathEmit(path: Path) extends (String => Unit) {
  def this(pathName: String) = this(Paths.get(pathName))

  Files.createDirectories(path.getParent)
  Files.deleteIfExists(path)

  override def apply(line: String): Unit = {
    Files.write(path, Seq(line).asJava, GlobalConstants.charset, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
  }
}
