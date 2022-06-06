package viewmodels.checkAnswers

import controllers.routes
import models.{CheckMode, UserAnswers}
import pages.UkAddressPage
import play.api.i18n.Messages
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.govukfrontend.views.Aliases.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import viewmodels.govuk.summarylist._
import viewmodels.implicits._

object $className$Summary  {

  def row(answers: UserAnswers)(implicit messages: Messages): Option[SummaryListRow] =
    answers.get($className$Page).map {
      answer =>

        def escapeValue(value:String) = HtmlFormat.escape(value).toString

        val value = Seq(Some(escapeValue(answer.$field1Name$)),answer.$field2Name$.map(escapeValue),
          Some(escapeValue(answer.$field3Name$)), answer.$field4Name$.map(escapeValue),
          Some(escapeValue(answer.$field5Name$))).flatten.mkString("<br/>")

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
