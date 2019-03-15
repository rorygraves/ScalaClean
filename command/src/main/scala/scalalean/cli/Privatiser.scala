package scalalean.cli

object Privatiser extends AbstractApp(new PrivatiserCli){

  init()
//  val model = buildAnalysisModel

}

import org.kohsuke.args4j.{Option => cliOption}
class PrivatiserCli extends BasicCli {
  @cliOption(name="consider-public") private var isPublic: Array[String] = Array()
}
