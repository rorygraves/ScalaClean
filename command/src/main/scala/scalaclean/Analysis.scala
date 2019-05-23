package scalaclean

import scalaclean.model.{ModelHelper, ParseModel, ScalaCleanModel}
import scalafix.v1._


class Analysis(storagePath: String, projectName: String, classpath: String, outputDir: String, relSrc: String, absSrc: String) {
  val model: ParseModel =  ScalaCleanModel.createParseModel

  def afterComplete(): Unit = {
    model.finishedParsing()
    val projectModel = model.asProjectModel(storagePath, projectName, classpath, outputDir, relSrc,absSrc)

    ModelHelper.model = Some(projectModel)
    println(s"Analysis AFTER COMPLETE size = ${projectModel.size}")
  }

  def analyse(implicit doc: SemanticDocument): Unit = {
    model.analyse(doc)
  }
}
