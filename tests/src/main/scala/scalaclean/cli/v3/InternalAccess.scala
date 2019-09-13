package scalaclean.cli.v3

import java.net.URLClassLoader

import scalaclean.model.reflect.GlobalHelper
import scalafix.internal.v1.InternalSemanticDoc
import scalafix.v1.SemanticDocument

import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.io._
import scala.reflect.runtime

/** hooks into the internals of semanticdb or scalameta
  *
  */
class InternalAccess {
  def reflectSymbol(cls: ParsedClassLike) = {
    val symbol = cls.globalSsmbol
      val doc = cls.doc
    val jm = mirror(doc)
    val fullName = symbol.toString
    //.asInstanceOf[JavaUniverse#Mirror]
    val javaName = fullName.
      substring(0, fullName.length - 1).
      replace('#', '.').
      replace('.', '.').
      replace('/', '.')
    val res = if (cls.isInstanceOf[ParsedObjectImpl]) jm.getRequiredModule(javaName)
    else jm.getRequiredClass(javaName)
    res
  }


  private[v2] val ru = runtime.universe.asInstanceOf[runtime.JavaUniverse]
  ru.settings.debug.value = true
  private[v2] val globalHelper = new GlobalHelper(ru)

  private val internalAccess = classOf[SemanticDocument].getMethod("internal")
  private val classpathAccess = classOf[GlobalSymbolTable].getDeclaredField("classpath")
  classpathAccess.setAccessible(true)

  def internalDoc(doc: SemanticDocument) =
    internalAccess.invoke(doc).asInstanceOf[InternalSemanticDoc]

  private val cachedClassLoader = new java.util.IdentityHashMap[GlobalSymbolTable, ru.JavaMirror]

  def mirror(doc: SemanticDocument) = {
    val symbolTable = internalDoc(doc).symtab.asInstanceOf[GlobalSymbolTable]
    cachedClassLoader.computeIfAbsent(symbolTable, {
      symbolTable =>

        val classpath = classpathAccess.get(symbolTable).asInstanceOf[Classpath]
        val urls = classpath.shallow.map {
          path => path.toURI.toURL
        }
        val cl = new URLClassLoader(urls.toArray)
        ru.runtimeMirror(cl)
    })
  }

}
