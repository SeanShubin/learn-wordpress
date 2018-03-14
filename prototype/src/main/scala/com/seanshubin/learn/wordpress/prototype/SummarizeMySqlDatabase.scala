package com.seanshubin.learn.wordpress.prototype

import java.sql._

object SummarizeMySqlDatabase extends App {
  val reportPath = args(0)
  val databaseName = args(1)
  val databasePassword = args(2)
  val emit = new CompositeEmit(new PathEmit(reportPath), println)

  withConnection{ conn =>
    val showTables: Iterator[String] = conn.exec("show tables", rowToString)
    val tableContentsLines = showTables.flatMap(x => tableContents(conn, x))
    tableContentsLines.foreach(emit)
  }

  def withConnection[T](connectionFunction: ConnectionOperations => T):T = {
//    Class.forName("com.mysql.jdbc.Driver")
    val conn =
      DriverManager.getConnection(
        s"jdbc:mysql://localhost:8889/$databaseName?serverTimezone=UTC", "root",databasePassword)
    try {
      connectionFunction(new ConnectionOperations(conn))
    } catch {
      case ex: SQLException =>
        val messageLines = Seq(
          s"SQLException: ${ex.getMessage}",
          s"SQLState: ${ex.getSQLState}",
          s"VendorError: ${ex.getErrorCode}"
        )
        val message = messageLines.mkString("\n")
        throw new RuntimeException(message, ex)
    } finally {
      conn.close()
    }
  }

  def formatRow(row:Seq[Any]):String = {
    row.map(_.toString).mkString(", ")
  }

  def rowToString(row:Seq[Any]):String = {
    val Seq(theCell) = row
    theCell.asInstanceOf[String]
  }

  def tableContents(connection:ConnectionOperations, table:String):Seq[String] = {
    val sql = s"select * from $table"
    val rowIterator = connection.exec(sql, x => x)
    val rowLines = TableUtil.createTable(
      rowIterator.columnNames +:
        rowIterator.columnTypes +:
        rowIterator.toSeq)
    val lines = sql +: rowLines
    lines
  }
}
