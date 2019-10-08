package scalaclean.cli

import java.nio.file.Paths

import scalaclean.rules.deadcode.DeadCodeRemover
import scalafix.testkit.DiffAssertions

class DeadCodeProjectTestRunner(val projectNames: List[String], overwriteTargetFiles: Boolean) extends DiffAssertions {

  def run(): Boolean = {

    val propsFiles = projectNames.map { projectName =>
      val srcDir = Paths.get(s"testProjects/$projectName/target/scala-2.12/classes/META-INF/ScalaClean/").toAbsolutePath
      srcDir.resolve(s"ScalaClean.properties").toFile
    }

    val options = SCOptions("deadcode",true, true,false,propsFiles)
    val main = new ScalaCleanMain(options,model =>  new DeadCodeRemover(model, options.debug))
    !main.run()
  }
}
