package com.lucidchart.xtractexample

import com.lucidchart.open.xtract.XmlReader

import scala.xml.XML

object Main {

  private val xml = XML.load(getClass.getResourceAsStream("/blog.xml"))

  def main(args: Array[String]): Unit = {
    val parsedBlog = XmlReader.of[Blog].read(xml)
    println("Parsed Result:")
    println(parsedBlog)
  }
}
