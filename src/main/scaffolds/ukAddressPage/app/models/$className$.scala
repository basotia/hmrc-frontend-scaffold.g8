package models

import play.api.libs.json._
import scala.annotation.tailrec
import utils.ModelHelpers._

case class Country(code: String, name: String)
object Country {
  implicit val format: OFormat[Country] = Json.format[Country]
}

case class $className$(
                        $field1Name$: String,//line1
                        $field2Name$: Option[String],//line2
                        $field3Name$: String,//townOrCity
                        $field4Name$: Option[String],//country
                        $field5Name$: String//postCode
                    )  {

  implicit val format = Json.format[$className$]
  val country: Country = Country("GB", "United Kingdom")

  def apply( line1: String,
             line2: Option[String],
             townOrCity: String,
             county: Option[String],
             postCode: String): $className$ = new $className$(normaliseSpaces(line1),
    normaliseSpaces(line2),
    normaliseSpaces(townOrCity),
    normaliseSpaces(county),
    normaliseSpaces(postCode))

}

object $className$ {
  implicit val reads: Reads[$className$] = {

    import play.api.libs.functional.syntax._

    (__ \ "country" \ "code").read[String].flatMap[String] {
      t =>
        if (t == "GB") Reads(_ => JsSuccess(t)) else Reads(_ => JsError("countryCode must be GB"))
    }.andKeep(
      (
        (__ \ "line1").read[String].map(normaliseSpaces) and
          (__ \ "line2").readNullable[String].map(normaliseSpaces) and
          (__ \ "townOrCity").read[String].map(normaliseSpaces) and
          (__ \ "county").readNullable[String].map(normaliseSpaces) and
          (__ \ "postCode").read[String].map(normaliseSpaces)
        )($className$(_, _, _, _, _)
      ))
  }

  implicit val writes: OWrites[$className$] = new OWrites[$className$] {

    override def writes(o: $className$): JsObject = {
      val line2Obj = o.line2.map(x => Json.obj("line2" -> x)).getOrElse(Json.obj())
      val countyObj = o.county.map(x => Json.obj("county" -> x)).getOrElse(Json.obj())

      Json.obj(
        "line1" -> o.$field1Name$,
        "townOrCity" -> o.$field3Name$,
        "country" -> o.country,
        "postCode" -> o.$field5Name$
      ) ++ line2Obj ++ countyObj
    }
  }


  def normaliseSpaces(string: String): String = {

    @tailrec
    def removeDoubleSpaces(string: String): String = {
      if(!string.contains("  ")) {
        string
      } else {
        removeDoubleSpaces(string.replaceAll("[ ]{2}", " "))
      }
    }

    removeDoubleSpaces(string.trim)
  }

  def normaliseSpaces(string: Option[String]): Option[String] = string.map(normaliseSpaces)
}