package scalaclean.fix

import scalaclean.model._
import scalaclean.model.impl.SourceModelImpl
import scalafix.patch.Patch
import scalafix.v1.SyntacticDocument

import scala.collection.immutable
import scala.meta.tokens.Token

object NewTreeVisitor {
  val continue: (Patch, Boolean) = (Patch.empty, true)
  val skip: (Patch, Boolean) = (Patch.empty, false)
}

abstract class NewTreeVisitor {
  private var _source:SourceModelImpl = _
  private var _doc: SyntacticDocument = _
  def tokensWithin(modelElement: ModelElement): immutable.IndexedSeq[Token] = {
    syntacticDocument.tokens.filter{
      token => token.start >= modelElement.rawStart && token.end <= modelElement.rawEnd
    }
  }


  protected final def syntacticDocument: SyntacticDocument = {
    import scala.meta.inputs.Input
    if (_doc == null) {
      val input = Input.File(_source.filename)
      SyntacticDocument.fromInput(input)
    }
    _doc
  }
  protected final def source: SourceModel = _source

  protected final def continue: (Patch, Boolean) = NewTreeVisitor.continue
  protected final def skip: (Patch, Boolean) = NewTreeVisitor.skip

  final def visitSourceFile(source: SourceModelImpl): Patch = {
    this._source = source
    val patch = handle(source, visitElement(source))
    this._source = null
    this._doc = null
    afterSource()
    patch
  }

  private def visitChildren(e: ModelElement): Patch = {
    e.directChildren.foldLeft(Patch.empty) {
      case (patch, child) =>
        patch + handle(child, visitElement(child))
    }
  }

  private def handle(e: ModelElement, handleRes: (Patch, Boolean)): Patch = {
    val (patch, traverseChildren) = handleRes
    if (traverseChildren)
      patch + visitChildren(e)
    else
      patch
  }

  protected def afterSource() = {}
  protected def visitElement(element: ModelElement): (Patch, Boolean)
}
