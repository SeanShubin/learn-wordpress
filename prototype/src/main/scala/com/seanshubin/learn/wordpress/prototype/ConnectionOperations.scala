package com.seanshubin.learn.wordpress.prototype

import java.sql.{CallableStatement, Connection, ResultSet}

class ConnectionOperations(val connection: Connection) {
  def exec[T](sql: String, rowToValue: Seq[Any] => T): ResultSetIteratorAsSeq[T] = {
    val statement: CallableStatement = connection.prepareCall(sql)
    statement.execute()
    val resultSet: ResultSet = statement.getResultSet
    new ResultSetIteratorAsSeq(resultSet, rowToValue)
  }
}
