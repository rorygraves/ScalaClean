package scalaclean.util

object FileHelper {

  def home: String = System.getProperty("user.home")

  def fileSep: String = System.getProperty("file.separator")

  def pathSep: String = System.getProperty("path.separator")


  def toPlatform(fileOrPath: String): String = {
    var res = fileOrPath

    res = res.replace("/", fileSep)
    res = res.replace("|", pathSep)
    res = res.replace("$HOME$", home)

    res

  }
}
