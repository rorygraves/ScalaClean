package scalaclean.model.impl.v2

import scalaclean.model._
import scalafix.v1.{SemanticDocument, Symbol}


import scala.collection.mutable

class ParserImpl extends ParseModel {

  def analyse(implicit doc: SemanticDocument) = new ParserTreeWalker(this, doc).analyse
  def finishedParsing(): Unit = {}

  override def asProjectModel: ProjectModel = ???

  private[v2] val internalAccess = new InternalAccess()
  private[v2] val additionalDataBuilder = new ExtraParsedData(this)


  private val bySymbol = mutable.Map[Symbol, ParsedElement]()
  private[v2] def record( element: ParsedElement): Unit = {
    assert(bySymbol.put(element.symbol, element).isEmpty, s"${element.symbol} enclosing ${element.enclosing} and =$element")
  }

}