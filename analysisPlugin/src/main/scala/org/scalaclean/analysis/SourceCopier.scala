package org.scalaclean.analysis

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import java.util.jar.JarOutputStream
import java.util.zip.ZipEntry

trait SourceCopier extends AutoCloseable {
  def copySource(sourceSymbol: ModelSource, content: String): Unit
}

object NoSourceCopier extends SourceCopier {
  override def copySource(sourceSymbol: ModelSource, content: String): Unit = ()

  override def close(): Unit = ()
}

class JarSourceCopier(jarPath: Path) extends SourceCopier {

  var _jarWriter: JarOutputStream = null

  private def jarWriter: JarOutputStream = {
    if (_jarWriter == null) {
      _jarWriter = new JarOutputStream(Files.newOutputStream(jarPath))
      _jarWriter.setLevel(9)
      _jarWriter.setMethod(ZipEntry.DEFLATED)
    }
    _jarWriter
  }

  override def copySource(sourceSymbol: ModelSource, content: String): Unit = {
    try {
      val path = sourceSymbol.common.sourceFile
      assert(path.isAbsolute, path.toString)
      val name = path.getRoot.relativize(path).toString
      val ze = new ZipEntry(name)
      val jar = jarWriter
      val bytes = content.getBytes(StandardCharsets.UTF_8)
      ze.setSize(bytes.length)
      jar.putNextEntry(ze)
      jar.write(bytes)
      jar.closeEntry()
    } catch {
      case e: Exception =>
        e.printStackTrace()
        e.printStackTrace()
        e.printStackTrace()
        e.printStackTrace()
    }

  }

  override def close(): Unit = if (_jarWriter ne null) {
    _jarWriter.close()
  }

}
