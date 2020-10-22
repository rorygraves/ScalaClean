package org.scalaclean.analysis

//names of the keys in the project properties
object PropertyNames {
  val prop_classpath         = "classpath"
  val prop_outputDir         = "outputDir"
  val prop_elementsFile      = "elementsFile"
  val prop_relationshipsFile = "relationshipsFile"
  val prop_extensionsFile    = "extensionsFile"

  // Contains source/test roots of what has been compiled.  (Blue/Green folders in Intellij)
  // or "/" to make all the files absolute
  val prop_srcRoots =    "srcRoots"

  val prop_srcBuildBase = "srcBuildBase"
  val prop_srcFiles     = "srcFiles"

  val prop_sourceOsPathSeparator = "sourcePathSep" // The path separator on the os where metadata was build. ":" of ";"
  val prop_sourceOsDirSeparator =
    "sourceDirSep" // The directory separator on the os where metadata was build. ":" of ";"
}
