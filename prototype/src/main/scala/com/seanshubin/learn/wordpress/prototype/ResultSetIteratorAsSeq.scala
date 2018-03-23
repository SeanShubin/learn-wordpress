package com.seanshubin.learn.wordpress.prototype

import java.sql.{ResultSet, SQLException}

class ResultSetIteratorAsSeq[T](resultSet: ResultSet, rowToValue: Seq[Any] => T) extends Iterator[T] {
  private val columnCount = resultSet.getMetaData.getColumnCount
  val columnNames: Seq[String] = (1 to columnCount).map(getColumnName)
  val columnTypes: Seq[String] = (1 to columnCount).map(getColumnType)
  private var maybeCurrent: Option[Seq[Any]] = advance()

  override def hasNext: Boolean = maybeCurrent.isDefined

  override def next(): T = {
    maybeCurrent match {
      case Some(current) =>
        maybeCurrent = advance()
        rowToValue(current)
      case None =>
        throw new RuntimeException("Attempt to read past end of result set")
    }
  }

  private def advance(): Option[Seq[Any]] = {
    if (resultSet.next()) {
      Some(resultSetToRow())
    } else {
      None
    }
  }

  private def resultSetToRow(): Seq[Any] = {
    (1 to columnCount).map(indexToCell)
  }

  private def indexToCell(index: Int): Any = {
    try {
      resultSet.getObject(index) match {
        case x: String =>
          if (x.length > 100) StringUtil.truncate(StringUtil.escape(x), 100)
          else StringUtil.escape(x)
        case x => x
      }
    } catch {
      case ex: SQLException =>
        s"<${ex.getMessage}>"
    }
  }

  private def getColumnName(index: Int): String = {
    resultSet.getMetaData.getColumnName(index)
  }

  private def getColumnType(index: Int): String = {
    resultSet.getMetaData.getColumnClassName(index)
  }
}
