package com.seanshubin.learn.wordpress.prototype

import scala.annotation.tailrec

object StringUtil {
  def escape(target: String) = {
    target.flatMap {
      case '\n' => "\\n"
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '\"' => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case x => x.toString
    }
  }

  def doubleQuote(target: String) = s""""${escape(target)}""""

  def unescape(target: String): String = {
    @tailrec
    def unescapeRemaining(soFar: String, remain: String): String = {
      if (remain.isEmpty) {
        soFar
      } else {
        val ch = remain.head
        if (ch == '\\') {
          unescapeAfterBackslash(soFar, remain.tail)
        } else {
          unescapeRemaining(soFar + ch, remain.tail)
        }
      }
    }

    def unescapeAfterBackslash(soFar: String, remain: String): String = {
      if (remain.isEmpty) {
        throw new RuntimeException("end of string encountered after backslash")
      } else {
        val ch = remain.head
        val escapeCh = ch match {
          case 'n' => '\n'
          case 'b' => '\b'
          case 't' => '\t'
          case 'f' => '\f'
          case 'r' => '\r'
          case '\"' => '\"'
          case '\'' => '\''
          case '\\' => '\\'
          case x => throw new RuntimeException("Unsupported escape sequence: " + ch)
        }
        unescapeRemaining(soFar + escapeCh, remain.tail)
      }
    }

    unescapeRemaining("", target)
  }

  def bytesToHex(target: Seq[Byte]): String = {
    target.map(byteToHex).mkString
  }

  def byteToHex(target: Byte): String = {
    val digits = "0123456789ABCDEF"
    "" + digits(target >> 4 & 15) + digits(target & 15)
  }

  def hexToBytes(target: String): Seq[Byte] = {
    target.grouped(2).map(hexToByte).toSeq
  }

  def hexToByte(rawTarget: String): Byte = {
    if (rawTarget.length != 2) throw new RuntimeException("hexToByte expects exactly 2 characters")
    val target = rawTarget.toUpperCase
    val numericTuples = 0 to 9 map (x => x.toString.head -> x)
    val alphaTuples = 'A' to 'F' map (x => x -> (x - 'A' + 10))
    val digitsMap = (numericTuples ++ alphaTuples).toMap
    val firstDigit = digitsMap(target(0))
    val secondDigit = digitsMap(target(1))
    val result = firstDigit * 16 + secondDigit
    result.toByte
  }

  def toLines(s: String): Seq[String] = {
    s.split("\r\n|\r|\n")
  }

  def truncate(s: String, max: Int): String = {
    if (s.length > max) {
      s"<${s.length} characters, showing first $max> ${s.substring(0, max)}"
    } else {
      s
    }
  }
}
