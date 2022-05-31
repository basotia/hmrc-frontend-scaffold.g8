package models

//case class $className$ ($field1Name$: String, $field2Name$: String)
//
//object $className$ {
//  implicit val format = Json.format[$className$]
//}

//sealed trait Address


case class $className$(
                        $field1Name$: String,//line1
                        $field2Name$: Option[String],//line2
                        $field3Name$: String,//townOrCity
                        $field4Name$: Option[String],//country
                        $field5Name$: String//postCode
                    )  {

//  def reads: Reads[Address] =
//    UkAddress.reads.widen[Address] orElse
//      InternationalAddress.format.widen[Address] orElse
//      DesAddress.format.widen[Address]
//
//  def writes: Writes[Address] = Writes {
//    case u: UkAddress            => Json.toJson(u)(UkAddress.writes)
//    case d: DesAddress           => Json.toJson(d)(DesAddress.format)
//    case i: InternationalAddress => Json.toJson(i)(InternationalAddress.format)
//  }

//  implicit def format: Format[Address] = Format(reads, writes)

  implicit val format = Json.format[$className$]
  val country: Country = Country("GB", "United Kingdom")

  def apply( line1: String,
             line2: Option[String],
             townOrCity: String,
             county: Option[String],
             postCode: String): UkAddress = new UkAddress(normaliseSpaces(line1),
    normaliseSpaces(line2),
    normaliseSpaces(townOrCity),
    normaliseSpaces(county),
    normaliseSpaces(postCode))

}

object UkAddress {
  implicit val reads: Reads[UkAddress] = {

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
        )(UkAddress(_, _, _, _, _)
      ))
  }

  implicit val writes: OWrites[UkAddress] = new OWrites[UkAddress] {

    override def writes(o: UkAddress): JsObject = {
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
}