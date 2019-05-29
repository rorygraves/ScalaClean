package scalaclean.cli

import scala.meta.io.AbsolutePath

object FileHelper {

  def home = System.getProperty("user.home")
  def homePath = AbsolutePath(home)
  def fileSep = System.getProperty("file.separator")
  def pathSep = System.getProperty("path.separator")


  def toPlatform(fileOrPath: String) = {
    var res = fileOrPath

    res = res.replace("/", fileSep)
    res = res.replace("|", pathSep)
    res = res.replace("$HOME$", home)

    res

  }
}
