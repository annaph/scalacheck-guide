package org.scalacheck.guide.foundation

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StringUtilsJUnitTest {

  @Test
  def testTruncateShorString(): Unit = {
    val s = "abc"
    val n = 5

    val actual = StringUtils.truncate(s, n)

    assertEquals(s, actual)
  }

  @Test
  def testTruncateLongString(): Unit = {
    val s = "Hello Anna"
    val n = 8

    val expected = "Hello An..."
    val actual = StringUtils.truncate(s, n)

    assertEquals(expected, actual)
  }

  @Test
  def testTokenize(): Unit = {
    val s = "foo;bar;42"
    val delim = ';'

    val expected = Array("foo", "bar", "42")
    val actual = StringUtils.tokenize(s, delim)

    assertTrue(actual sameElements expected)
  }

  @Test
  def testTokenizeSingle(): Unit = {
    val s = "Hello Anna"
    val delim = ','

    val expected = Array(s)
    val actual = StringUtils.tokenize(s, delim)

    assertTrue(actual sameElements expected)
  }

  @Test
  def testContainsTrue(): Unit = {
    val s = "abc"
    val subString = "bc"

    val actual = StringUtils.contains(s, subString)

    assertTrue(actual)
  }

  @Test
  def testContainsFalse(): Unit = {
    val s = "abc"
    val subString = "42"

    val actual = StringUtils.contains(s, subString)

    assertFalse(actual)
  }

}
