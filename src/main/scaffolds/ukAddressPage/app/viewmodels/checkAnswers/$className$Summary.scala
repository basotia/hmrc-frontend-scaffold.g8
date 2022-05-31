package viewmodels.checkAnswers

import controllers.routes
import models.{CheckMode, UserAnswers}
import pages.$className$Page
import play.api.i18n.Messages
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import viewmodels.govuk.summarylist._
import viewmodels.implicits._

object $className$Summary  {

  def row(answers: UserAnswers)(implicit messages: Messages): Option[SummaryListRow] =
    answers.get($className$Page).map {
      answer =>

        def escapeValue(value:String) = HtmlFormat.escape(value).toString

        val value = escapeValue(answer.$field1Name$) + "<br/>"+ escapeValue(answer.$field2Name$)+ "<br/>" +
                    escapeValue(answer.$field3Name$)+ "<br/>" + escapeValue(answer.$field4Name$)+ "<br/>" +
                    escapeValue(answer.$field5Name$)

        SummaryListRowViewModel(
          key     = "$className;format="decap"$.checkYourAnswersLabel",
          value   = ValueViewModel(HtmlContent(value)),
          actions = Seq(
            ActionItemViewModel("site.change", routes.$className$Controller.onPageLoad(CheckMode).url)
              .withVisuallyHiddenText(messages("$className;format="decap"$.change.hidden"))
          )
        )
    }
}
