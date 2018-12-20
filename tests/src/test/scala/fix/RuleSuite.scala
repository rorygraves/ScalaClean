package fix

import scalafix.testkit.SemanticRuleSuite

class RuleSuite extends SemanticRuleSuite() {
  // The logic here looks for all files in the input directory and runs them as tests.
  // the config over which rules to run is defined at the top of the file.

  runAllTests()
}
