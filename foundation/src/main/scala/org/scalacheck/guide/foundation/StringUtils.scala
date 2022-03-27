package org.scalacheck.guide.foundation

object StringUtils {

  def truncate(s: String, n: Int): String = {
    if (n < 0) ""
    else if (s.length <= n) s
    else s"${s take n}..."
  }

  def tokenize(s: String, delim: Char): Array[String] =
    s.split(delim).filter(_.nonEmpty)

  def contains(s: String, subString: String): Boolean =
    s.indexOf(subString) >= 0

}
