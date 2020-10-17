package org.scalaclean.analysis

//names of the keys in the project properties
object PropertyNames {
  val prop_classpath         = "classpath"
  val prop_outputDir         = "outputDir"
  val prop_elementsFile      = "elementsFile"
  val prop_relationshipsFile = "relationshipsFile"
  val prop_extensionsFile    = "extensionsFile"

  val prop_srcRoots =
    "srcRoots" // Contains source/test roots of what has been compiled.  (Blue/Green folders in Intellij)
  val prop_srcBuildBase = "srcBuildBase"
  val prop_srcFiles     = "srcFiles"

  val prefix_option = "option."

  val prop_sourceOsPathSeparator = "sourcePathSep" // The path separator on the os where metadata was build. ":" of ";"
  val prop_sourceOsDirSeparator =
    "sourceDirSep" // The directory separator on the os where metadata was build. ":" of ";"
}
