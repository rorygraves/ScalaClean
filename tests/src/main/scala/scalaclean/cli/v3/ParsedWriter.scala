package scalaclean.cli.v3

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import scala.meta.inputs.Input


class ParsedWriter(path: Path, projectRoot: Path) {
  val dir = projectRoot.resolve(path).toAbsolutePath
  println(s" writing to dir $dir")
  Files.createDirectories(dir)
  val ele = dir.resolve(IoTokens.fileElements).toAbsolutePath
  val rels= dir.resolve(IoTokens.fileRelationships ).toAbsolutePath
  println(s" writing elements $ele")
  val elementsFile = Files.newBufferedWriter(ele,
    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.CREATE)
  println(s" writing relationships $rels")
  val relationshipsFile = Files.newBufferedWriter(rels,
    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.CREATE)

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
        writeElementImpl(e, IoTokens.typeClass)
        writeClassLike(e)
        writeClassImpl(e)
        writeEnd()
      case e: ParsedMethod =>
        writeElementImpl(e, IoTokens.typeMethod)
        writeMethod(e)
        writeEnd()
      case e: ParsedVal =>
        writeElementImpl(e, IoTokens.typeVal)
        writeField(e)
        writeVal(e)
        writeEnd()
      case e: ParsedVar =>
        writeElementImpl(e, IoTokens.typeVar)
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
      case file: Input.VirtualFile => Paths.get(file.path)
      case x  =>
        throw new IllegalStateException(s"unexpected input type ${x.getClass}")
    }
    val source = e.key.toCsv
    elementsFile.write(s"$typeId,${source},${inputFile},${e.stat.pos.start},${e.stat.pos.end}")

    e.enclosing foreach { parent =>
      relationshipsFile.write(s"${source},${IoTokens.relWithin},${parent.key.toCsv}")
      relationshipsFile.newLine()
    }
    e.refersTo foreach { case (to, synthetic) =>
      relationshipsFile.write(s"${source},${IoTokens.relRefers},${to.toCsv},$synthetic")
      relationshipsFile.newLine()
    }
    val directOverrides = e.directOverrides
    val transOverrides = e.transitiveOverrides
    transOverrides foreach { over =>
      val isDirect = directOverrides.contains(over)
      relationshipsFile.write(s"${source},${IoTokens.relOverrides},${over.toCsv},$isDirect")
      relationshipsFile.newLine()
    }
  }
  private def writeClassLike(c: ParsedClassLike): Unit = {

    val directExtends = c.directExtends
    val transExtends = c.transitiveExtends

    transExtends foreach {
      ext =>
        val isDirect = directExtends.contains(ext)
        relationshipsFile.write(s"${c.key.toCsv},${IoTokens.relExtends},${ext.toCsv},$isDirect")
        relationshipsFile.newLine()
    }

  }
  private def writeObjectImpl(o: ParsedObjectImpl): Unit = {}
  private def writeTraitImpl(o: ParsedTraitImpl): Unit = {}
  private def writeClassImpl(o: ParsedClassImpl): Unit = {}
  private def writeMethod(o: ParsedMethod): Unit = {
    elementsFile.write(s",${o.isAbstract},${o.method_name},${o.method_decltpe.isDefined}")
  }
  private def writeField(o: ParsedField): Unit = {
    elementsFile.write(s",${o.isAbstract},${o.field.name}")
  }
  private def writeVal(o: ParsedVal): Unit = {
    elementsFile.write(s",${o.isLazy}")
  }
  private def writeVar(o: ParsedVar): Unit = {}

  def close(): Unit = {
    elementsFile.flush()
    relationshipsFile.flush()

    elementsFile.close()
    relationshipsFile.close()
  }

}
