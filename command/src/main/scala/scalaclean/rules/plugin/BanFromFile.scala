package scalaclean.rules.plugin

import java.nio.file.{Files, Path, Paths}

import scalaclean.model.{ElementIds, Mark, ModelElement, SimpleReason}
import scalaclean.rules.RuleRun

import scala.util.matching.Regex

object BanFromFile extends RulePluginFactory {
  override def build(params: String): BanFromFile = new BanFromFile(Paths.get(params))

}
class BanFromFile(filename: Path) extends RulePlugin {
  override def name: String = s"BanFromFile - $filename"

  override def beforeMainRule(mainRule: RuleRun[_]): Unit = {
    val reader = Files.newBufferedReader(filename)
    try {
      reader.lines().forEach(s =>
        s.split(",",3) match {
          case Array("single", element, comment) =>
            mainRule.model.getElement[ModelElement](ElementIds(element)) match {
              case Some(e) => e.mark = Mark.dontChange[mainRule.SpecificColour](SimpleReason(s"BanFromFile - $comment"))
              case None =>
            }
          case Array("pattern", pattern, comment) =>
            val regex = new Regex(pattern)
            val mark = Mark.dontChange[mainRule.SpecificColour](SimpleReason(s"BanFromFile - $comment"))

            mainRule.model.allOf[ModelElement].filter{
              ele =>
                regex.findFirstIn(ele.modelElementId.toString).nonEmpty
              }.foreach(_.mark = mark)
          case other =>
            throw new Exception(s"BanFromFile cant parse '$s'")
        })
    } finally reader.close
  }

  override def afterMainRule(mainRule: RuleRun[_]): Unit = ()
}
