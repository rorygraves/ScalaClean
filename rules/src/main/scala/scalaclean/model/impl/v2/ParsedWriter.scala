package scalaclean.model.impl.v2

import java.io.BufferedWriter
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.meta.inputs.Input


class ParsedWriter(path: Path, projectRoot: Path) {
  val elementsFile = Files.newBufferedWriter(path.resolve(IoTokens.fileElements),
    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE)
  val relationshipsFile = Files.newBufferedWriter(path.resolve(IoTokens.fileRelationships ),
    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE)

  def writeElement(element: ParsedElement): Unit = {
    element match {
      case e: ParsedObjectImpl =>
        writeElementImpl(e, IoTokens.typeObject)
        writeClassLike(e)
        writeObjectImpl(e)
        writeEnd()
      case e: ParsedTraitImpl =>
        writeElementImpl(e, IoTokens.typeTrait)
        writeClassLike(e)
        writeTraitImpl(e)
        writeEnd()
      case e: ParsedClassImpl =>
        writeElementImpl(e, IoTokens.typeTrait)
        writeClassLike(e)
        writeClassImpl(e)
        writeEnd()
      case e: ParsedMethod =>
        writeElementImpl(e, IoTokens.typeTrait)
        writeMethod(e)
        writeEnd()
      case e: ParsedVal =>
        writeElementImpl(e, IoTokens.typeTrait)
        writeField(e)
        writeVal(e)
        writeEnd()
      case e: ParsedVar =>
        writeElementImpl(e, IoTokens.typeTrait)
        writeField(e)
        writeVar(e)
        writeEnd()
    }
  }

  private def writeEnd(): Unit = {
    elementsFile.newLine()
  }
  private def writeElementImpl(e: ParsedElement, typeId: String): Unit = {
    val inputFile = e.doc.input match {
      case file: Input.File => file.path.toNIO.toAbsolutePath.relativize(projectRoot)
      case _  => ???
    }
    elementsFile.write(s"$typeId,${e.symbol.value},${inputFile},${e.stat.pos.start},${e.stat.pos.end}")

    e.enclosing foreach { parent =>
      relationshipsFile.write(s"${e.symbol.value},${IoTokens.relWithin},${parent.symbol.value}}")
      relationshipsFile.newLine()
    }
    e.refersTo foreach { case (to, synthetic) =>
      relationshipsFile.write(s"${e.symbol.value},${IoTokens.relRefers},${to.value}},${synthetic}}")
      relationshipsFile.newLine()
    }
    val directOverrides = e.directOverrides
    val transOverrides = e.transitiveOverrides
    transOverrides foreach { over =>
      val isDirect = directOverrides.contains(over)
      relationshipsFile.write(s"${e.symbol.value},${IoTokens.relOverrides},${over.value}},$isDirect")
      relationshipsFile.newLine()
    }
  }
  private def writeClassLike(c: ParsedClassLike): Unit = {

    val directExtends = c.directExtends
    val transExtends = c.transitiveExtends

    transExtends foreach {
      ext =>
        val isDirect = directExtends.contains(ext)
        relationshipsFile.write(s"${c.symbol.value},${IoTokens.relExtends},${ext.value}},$isDirect")
        relationshipsFile.newLine()
    }

  }
  private def writeObjectImpl(o: ParsedObjectImpl): Unit = {}
  private def writeTraitImpl(o: ParsedTraitImpl): Unit = {}
  private def writeClassImpl(o: ParsedClassImpl): Unit = {}
  private def writeMethod(o: ParsedMethod): Unit = {
    elementsFile.write(s",${o.isAbstract}},${o.method_name}},${o.method_decltpe.isDefined}}")
  }
  private def writeField(o: ParsedField): Unit = {
    elementsFile.write(s",${o.isAbstract}},${o.field.name}}")
  }
  private def writeVal(o: ParsedVal): Unit = {
    elementsFile.write(s",${o.isLazy}}")
  }
  private def writeVar(o: ParsedVar): Unit = {}

  def close(): Unit = {
    elementsFile.flush()
    relationshipsFile.flush()

    elementsFile.close()
    relationshipsFile.close()
  }

}
