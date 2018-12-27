package perceptor

import scalafix.testkit.SemanticRuleSuite

class PerceptorSuite extends SemanticRuleSuite() {

  override def runAllTests() = {
    testsToRun.filter(_.path.input.toString.contains("perceptor")).foreach(runOn)
  }

  runAllTests()

}
