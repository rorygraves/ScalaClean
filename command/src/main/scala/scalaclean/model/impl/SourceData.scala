package scalaclean.model.impl

import java.nio.file.Path

/**
  * represents a source file
  *
  * @param project the containing project
  * @param path    the relative path to the source from the project path
  */
case class SourceData(project: Project, path: Path) {
}
