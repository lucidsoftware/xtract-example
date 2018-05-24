package com.lucidchart.xtractexample

import com.lucidchart.open.xtract.{XmlReader, __}
import com.lucidchart.open.xtract.XmlReader._

import cats.syntax.all._

object BlogType extends Enumeration {
  val tech = Value("technical")
  val marketing = Value("marketing")
  val product = Value("product")
  val business = Value("business")
}

case class Blog(
  title: String,
  subtitle: Option[String],
  author: AuthorInfo,
  blogType: BlogType.Value,
  content: Content
)

object Blog {
  implicit val reader: XmlReader[Blog] = (
    (__ \ "head" \ "title").read[String],
    (__ \ "head"\ "subtitle").read[String].optional,
    (__ \ "head" \ "author").read[AuthorInfo],
    attribute("type")(enum(BlogType)).default(BlogType.tech),
    (__ \ "body").read[Content]
  ).mapN(apply _)
}

case class AuthorInfo(
  name: String,
  email: Option[String],
  department: Departments.Value,
  canContact: Boolean
)

object AuthorInfo {
  def validateEmail(email: String): Boolean = email contains "@"

  private val nameReader: XmlReader[String] = {
    for {
      first <- attribute[String]("first")
      last <- attribute[String]("last")
    } yield {
      first + " " + last
    }
  }
  implicit val reader: XmlReader[AuthorInfo] = (
    nameReader,
    attribute[String]("email").filter(validateEmail _).optional,
    attribute("department")(enum(Departments)),
    attribute[Boolean]("canContact"),
  ).mapN(apply _)
}

case class Content(sections: Seq[Section])

object Content {
  implicit val reader: XmlReader[Content] = (__ \ "section").read(seq[Section]).map(apply _)
}

case class Section(title: Option[String], paragraphs: Seq[String])

object Section {
  implicit val reader: XmlReader[Section] = (
    attribute[String]("title").optional,
    (__ \ "p").read(seq[String])
  ).mapN(apply _)
}
