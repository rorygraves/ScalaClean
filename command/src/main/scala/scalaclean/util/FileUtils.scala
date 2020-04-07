package scalaclean.util

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}

object FileUtils {
  def readAllBytes(path: Path): String = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)

}
