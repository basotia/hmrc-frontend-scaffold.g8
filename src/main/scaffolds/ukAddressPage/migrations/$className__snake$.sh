#!/bin/bash

echo ""
echo "Applying migration $className;format="snake"$"

echo "Adding routes to conf/app.routes"

echo "" >> ../conf/app.routes
echo "GET        /$className;format="decap"$                        controllers.$className$Controller.onPageLoad(mode: Mode = NormalMode)" >> ../conf/app.routes
echo "POST       /$className;format="decap"$                        controllers.$className$Controller.onSubmit(mode: Mode = NormalMode)" >> ../conf/app.routes

echo "GET        /change$className$                  controllers.$className$Controller.onPageLoad(mode: Mode = CheckMode)" >> ../conf/app.routes
echo "POST       /change$className$                  controllers.$className$Controller.onSubmit(mode: Mode = CheckMode)" >> ../conf/app.routes

echo "Adding messages to conf.messages"
echo "" >> ../conf/messages.en
echo "$className;format="decap"$.title = $className;format="decap"$" >> ../conf/messages.en
echo "$className;format="decap"$.heading = $className;format="decap"$" >> ../conf/messages.en
echo "$className;format="decap"$.$field1Name$ = $field1Name$" >> ../conf/messages.en
echo "$className;format="decap"$.$field2Name$ = $field2Name$" >> ../conf/messages.en
echo "$className;format="decap"$.$field3Name$ = $field3Name$" >> ../conf/messages.en
echo "$className;format="decap"$.$field4Name$ = $field4Name$" >> ../conf/messages.en
echo "$className;format="decap"$.$field5Name$ = $field5Name$" >> ../conf/messages.en
echo "$className;format="decap"$.checkYourAnswersLabel = $className$" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field1Name$.required = Enter $field1Name$" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field2Name$.required = Enter $field2Name$" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field3Name$.required = Enter $field3Name$" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field4Name$.required = Enter $field4Name$" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field5Name$.required = Enter $field5Name$" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field1Name$.length = $field1Name$ must be $field1MaxLength$ characters or less" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field2Name$.length = $field2Name$ must be $field2MaxLength$ characters or less" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field3Name$.length = $field3Name$ must be $field3MaxLength$ characters or less" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field4Name$.length = $field4Name$ must be $field4MaxLength$ characters or less" >> ../conf/messages.en
echo "$className;format="decap"$.error.$field5Name$.length = $field5Name$ must be $field5MaxLength$ characters or less" >> ../conf/messages.en
echo "$className;format="decap"$.$field1Name$.change.hidden = $field1Name$" >> ../conf/messages.en
echo "$className;format="decap"$.$field2Name$.change.hidden = $field2Name$" >> ../conf/messages.en
echo "$className;format="decap"$.$field3Name$.change.hidden = $field3Name$" >> ../conf/messages.en
echo "$className;format="decap"$.$field4Name$.change.hidden = $field4Name$" >> ../conf/messages.en
echo "$className;format="decap"$.$field5Name$.change.hidden = $field5Name$" >> ../conf/messages.en

echo "Adding to UserAnswersEntryGenerators"
awk '/trait UserAnswersEntryGenerators/ {\
    print;\
    print "";\
    print "  implicit lazy val arbitrary$className$UserAnswersEntry: Arbitrary[($className$Page.type, JsValue)] =";\
    print "    Arbitrary {";\
    print "      for {";\
    print "        page  <- arbitrary[$className$Page.type]";\
    print "        value <- arbitrary[$className$].map(Json.toJson(_))";\
    print "      } yield (page, value)";\
    print "    }";\
    next }1' ../test-utils/generators/UserAnswersEntryGenerators.scala > tmp && mv tmp ../test-utils/generators/UserAnswersEntryGenerators.scala

echo "Adding to PageGenerators"
awk '/trait PageGenerators/ {\
    print;\
    print "";\
    print "  implicit lazy val arbitrary$className$Page: Arbitrary[$className$Page.type] =";\
    print "    Arbitrary($className$Page)";\
    next }1' ../test-utils/generators/PageGenerators.scala > tmp && mv tmp ../test-utils/generators/PageGenerators.scala

echo "Adding to ModelGenerators"
awk '/trait ModelGenerators/ {\
    print;\
    print "";\
    print "  implicit lazy val arbitrary$className$: Arbitrary[$className$] =";\
    print "    Arbitrary {";\
    print "      for {";\
    print "        $field1Name$ <- arbitrary[String]";\
    print "        $field2Name$ <- arbitrary[String]";\
    print "        $field3Name$ <- arbitrary[String]";\
    print "        $field4Name$ <- arbitrary[String]";\
    print "        $field5Name$ <- arbitrary[String]";\
    print "      } yield $className$($field1Name$, Some($field2Name$), $field3Name$, Some($field4Name$), $field5Name$)";\
    print "    }";\
    next }1' ../test-utils/generators/ModelGenerators.scala > tmp && mv tmp ../test-utils/generators/ModelGenerators.scala

echo "Adding to UserAnswersGenerator"
awk '/val generators/ {\
    print;\
    print "    arbitrary[($className$Page.type, JsValue)] ::";\
    next }1' ../test-utils/generators/UserAnswersGenerator.scala > tmp && mv tmp ../test-utils/generators/UserAnswersGenerator.scala

echo "Migration $className;format="snake"$ completed"
