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
                        $field4Name$: Option[String],//county
                        $field5Name$: String//postCode
                    )  {

  implicit val format = Json.format[$className$]
  val country: Country = Country("GB", "United Kingdom")

  def apply( $field1Name$: String,
             $field2Name$: Option[String],
             $field3Name$: String,
             $field4Name$: Option[String],
             $field5Name$: String): $className$ = new $className$(normaliseSpaces($field1Name$),
    normaliseSpaces($field2Name$),
    normaliseSpaces($field3Name$),
    normaliseSpaces($field4Name$),
    normaliseSpaces($field5Name$))

}

object $className$ {
  implicit val reads: Reads[$className$] = {

    import play.api.libs.functional.syntax._

    (__ \ "country" \ "code").read[String].flatMap[String] {
      t =>
        if (t == "GB") Reads(_ => JsSuccess(t)) else Reads(_ => JsError("countryCode must be GB"))
    }.andKeep(
      (
        (__ \ $field1Name$).read[String].map(normaliseSpaces) and
          (__ \ $field2Name$).readNullable[String].map(normaliseSpaces) and
          (__ \ $field3Name$).read[String].map(normaliseSpaces) and
          (__ \ $field4Name$).readNullable[String].map(normaliseSpaces) and
          (__ \ $field5Name$).read[String].map(normaliseSpaces)
        )($className$(_, _, _, _, _)
      ))
  }

  implicit val writes: OWrites[$className$] = new OWrites[$className$] {

    override def writes(o: $className$): JsObject = {
      val line2Obj = o.$field2Name$.map(x => Json.obj($field2Name$ -> x)).getOrElse(Json.obj())
      val countyObj = o.$field4Name$.map(x => Json.obj($field4Name$ -> x)).getOrElse(Json.obj())

      Json.obj(
        $field1Name$ -> o.$field1Name$,
        $field3Name$ -> o.$field3Name$,
        "country" -> o.country,
        $field5Name$ -> o.$field5Name$
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