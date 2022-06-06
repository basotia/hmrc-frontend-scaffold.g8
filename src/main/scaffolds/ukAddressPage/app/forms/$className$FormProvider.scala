package forms

import javax.inject.Inject
import forms.mappings.Mappings
import play.api.data.Form
import play.api.data.Forms._
import models.$className$

class $className$FormProvider @Inject() extends Mappings {

   def apply(): Form[$className$] = Form(
     mapping(
      "$field1Name$" -> text("$className;format="decap"$.error.$field1Name$.required")
        .verifying(maxLength($field1MaxLength$, "$className;format="decap"$.error.$field1Name$.length")),
      "$field2Name$" -> optional(text("$className;format="decap"$.error.$field2Name$.required")
        .verifying(maxLength($field2MaxLength$, "$className;format="decap"$.error.$field2Name$.length"))),
      "$field3Name$" -> text("$className;format="decap"$.error.$field3Name$.required")
      .verifying(maxLength($field2MaxLength$, "$className;format="decap"$.error.$field3Name$.length")),
      "$field4Name$" -> optional(text("$className;format="decap"$.error.$field4Name$.required")
      .verifying(maxLength($field2MaxLength$, "$className;format="decap"$.error.$field4Name$.length"))),
      "$field5Name$" -> text("$className;format="decap"$.error.$field5Name$.required")
      .verifying(maxLength($field2MaxLength$, "$className;format="decap"$.error.$field5Name$.length"))
        )($className$.apply)($className$.unapply)
   )
 }
