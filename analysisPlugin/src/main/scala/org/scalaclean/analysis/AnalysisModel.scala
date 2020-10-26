package org.scalaclean.analysis

import java.util.concurrent.atomic.AtomicInteger

import scalaclean.model.ElementId

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.tools.nsc.Global

trait HasModelCommon {
  def common: ModelCommon

  def newCsvString: String = common.newCsvString
}

sealed trait ModelSymbol extends HasModelCommon {

  final def debugName = this match {
    case _: ModelVar          => "var"
    case _: ModelVal          => "val"
    case _: ModelFields       => "(compound fields)"
    case _: ModelGetterMethod => "getter"
    case _: ModelSetterMethod => "setter"
    case _: ModelPlainMethod  => "def"
    case _: ModelObject       => "object"
    case _: ModelClass        => "class"
    case _: ModelTrait        => "trait"
    case _: ModelSource       => "source"
  }

  val common: ModelCommon
  var traversal: Int = -1

  def isGlobal: Boolean = common.isGlobal

  def sourceFile: String = common.sourceFile

  def posStart: Int = common.posStart

  def posEnd: Int = common.posEnd

  def posFocus: Int = common.posFocus

  def isParameter: Boolean = false

  def sourceName: String = common.sourceName

  val tree: Global#Tree

  private var _extensionData: List[ExtensionData] = Nil

  def addExtensionData(additionalData: List[ExtensionData]): Unit = {
    if (_extensionData.isEmpty) _extensionData = additionalData
    else _extensionData = _extensionData.reverse_:::(additionalData)
  }

  def extensionData: List[ExtensionData] = _extensionData

  final def ioToken = this match {
    case _: ModelVar          => IoTokens.typeVar
    case _: ModelVal          => IoTokens.typeVal
    case _: ModelFields       => IoTokens.typeFields
    case _: ModelGetterMethod => IoTokens.typeGetterMethod
    case _: ModelSetterMethod => IoTokens.typeSetterMethod
    case _: ModelPlainMethod  => IoTokens.typePlainMethod
    case _: ModelObject       => IoTokens.typeObject
    case _: ModelClass        => IoTokens.typeClass
    case _: ModelTrait        => IoTokens.typeTrait
    case _: ModelSource       => IoTokens.typeSource
  }

  var children: Map[ModelCommon, ModelSymbol] = Map.empty
  def findChildBySymbol[T <: ModelSymbol : ClassTag](sym: Global#Symbol): Option[T] = {
    children.values.collectFirst{
      case ele if ele.tree.symbol == sym =>
        ele.asInstanceOf[T]
    }
  }

  var extendsRels: ListSet[(HasModelCommon, Boolean)]   = ListSet.empty
  var overridesRels: ListSet[(HasModelCommon, Boolean)] = ListSet.empty
  var refersRels: ListSet[(HasModelCommon, Boolean)]    = ListSet.empty
  var withinRels: ListSet[ModelSymbol]                  = ListSet.empty
  var gettersFor: ListSet[ModelCommon]                  = ListSet.empty
  var settersFor: ListSet[ModelCommon]                  = ListSet.empty

  def addGetterFor(field: ModelCommon): Unit = {
    gettersFor += field
  }

  def addSetterFor(field: ModelCommon): Unit = {
    settersFor += field
  }

  def addExtends(parentSym: HasModelCommon, direct: Boolean): Unit = {
    extendsRels = extendsRels.+((parentSym, direct))
  }

  def addOverride(common: ModelCommon, direct: Boolean): Unit = {
    overridesRels = overridesRels.+((common, direct))
  }

  def addRefers(common: ModelCommon, isSynthetic: Boolean): Unit = {
    refersRels = refersRels.+((common, isSynthetic))
  }

  def addWithin(mSymbol: ModelSymbol): Unit = {
    withinRels = withinRels.+(mSymbol)
  }

  def addChild(modelSymbol: ModelSymbol): Unit = {
    children = children + (modelSymbol.common -> modelSymbol)
  }

  def printStructure(): Unit = {
    printStructureInt(0)
  }

  private def printStructureInt(depth: Int): Unit = {
    if (depth > 0) {
      print("  " * (depth - 1))
      print("+-")
    }
    println(idWithDeDuplicationSuffix + " isLocal = " + !isGlobal + ", isParameter= " + isParameter)

    def printRelStart(): Unit = {
      if (depth > 0) {
        print("  " * (depth - 1))
        print("|   ")
      }
    }

    extendsRels.foreach { case (ext, direct) =>
      printRelStart()
      println(s"extends: $ext  direct=$direct")
    }

    overridesRels.foreach { case (ovr, direct) =>
      printRelStart()
      println(s"overrides: $ovr  direct=$direct")
    }

    withinRels.foreach { within =>
      printRelStart()
      println(s"within: ${within.newCsvString}")
    }

    refersRels.foreach { case (refers, isSynthetic) =>
      printRelStart()
      println(s"refers: ${refers.newCsvString}   isGlobal=${refers.common.isGlobal}")
    }

    gettersFor.foreach { getterTarget =>
      printRelStart()
      println(s"getterFor: ${getterTarget.newCsvString}")
    }

    settersFor.foreach { setterTarget =>
      printRelStart()
      println(s"setterFor: ${setterTarget.newCsvString}")
    }

    if (suffix != -1) {
      printRelStart()
      println(s"duplicate of : $newCsvString")
    }
    this match {
      case f : ModelField =>
        f.constructorParam.foreach{ p =>
          printRelStart()
          println(s"has associated constructor param  of : $p")
        }
        f.defaultGetter.foreach{ p =>
          printRelStart()
          println(s"has associated defaultGetter  of : $p")
        }
      case _ =>
    }

    children.values.foreach(child => child.printStructureInt(depth + 1))
  }

  def outputStructure(
      eleWriter: ElementsWriter,
      relWriter: RelationshipsWriter,
      extensionWriter: ExtensionWriter
  ): Unit = {
    eleWriter.write(this)
    extensionWriter.writeExtensions(this)

    extendsRels.foreach { case (ext, direct) =>
      relWriter.extendsCls(ext, this, direct)
    }

    overridesRels.foreach { case (ovr, direct) =>
      // bit of a hack with the cast - could be more specfic on subclasses
      relWriter.overrides(this.asInstanceOf[ModelMethod], ovr, direct)
    }

    withinRels.foreach(within => relWriter.within(within, this))

    refersRels.foreach { case (refers, isSynthetic) =>
      relWriter.refers(this, refers, isSynthetic)
    }

    gettersFor.foreach(getterTarget => relWriter.getterFor(this.common, getterTarget))

    settersFor.foreach(setterTarget => relWriter.setterFor(this.common, setterTarget))

    if (suffix != -1) relWriter.recordDuplicate(this)

    this match {
      case f : ModelField =>
        f.constructorParam.foreach(param => relWriter.relatedCtorParam(f, param))
        f.defaultGetter.foreach(getter => relWriter.defaultGetterMethod(f, getter))
      case _ =>
    }

    children.values.foreach(child => child.outputStructure(eleWriter, relWriter, extensionWriter))
  }

  def flatten(): Unit = {
    children = children.flatMap { case t @ (common, child) =>
      child.flatten()
      child match {
        //TODO move params of a global method to be global
        //its a parameter of a method
        case mf: ModelField
          if mf.isParameter &&
            mf.tree.symbol.owner == this.tree.symbol &&
            this.common.isGlobal && this.isInstanceOf[ModelMethod]
        =>
          //probably not in the long term
          this.refersRels ++= mf.refersRels
          Some(t)
        case mf: ModelField if mf.isParameter || !mf.isGlobal =>
          this.refersRels ++= mf.refersRels
          None
        case _ =>
          Some(t)
      }
    }
    val duplicateGroups = children.values.groupBy(_.common.elementId).filter(_._2.size > 1)
    val suffix = new AtomicInteger
    for (duplicate: Iterable[ModelSymbol] <- duplicateGroups.values.toList) {
      suffix.set(0)
      def keep(symToKeep: ModelSymbol): Unit = {
        duplicate filter (_ ne symToKeep) foreach (_.markDuplicateOf(symToKeep, suffix.incrementAndGet()))
      }
      val preferred = duplicate filter (!_.tree.symbol.isSynthetic)
      preferred.size match {
        case 1 =>
          println(s"flatten - found preferred - $newCsvString, ${duplicate.size}")
          keep(preferred.head)
        case 0 =>
          println(s"flatten - cant find any preferred - $newCsvString, ${duplicate.size}")
          keep (duplicate.head)
        case n =>
          //try harder ??
          println(s"flatten - cant find single preferred - $newCsvString, ${duplicate.size}")
          keep (duplicate.head)
      }
    }
    refersRels = refersRels.filter(_._1.common.elementId != this.common.elementId)

  }
  // -1 means no duplicate
  var suffix: Int = -1
  def markDuplicateOf(mainDeDup: ModelSymbol, suffix: Int): Unit = {
    this.suffix = suffix
  }
  def idWithDeDuplicationSuffix = if (suffix == -1) newCsvString else s"${newCsvString}--$suffix"


}

sealed trait ClassLike extends ModelSymbol {
  private var postProcessing = List.empty[() => Unit]

  def addPostProcess(fn: () => Unit): Unit = postProcessing ::= fn

  def postProcess(): Unit = {
    postProcessing.foreach(_.apply())
  }

  /**
   * the overrides that have not yet been processed
   * After the ClassLike is processed this is  the overrides for each of the def/val in the class
   * As they are processed they are removed, and the remainder are processed after the class
   * is completed
   */
  val remainingChildOverrides: mutable.Map[Global#Symbol, mutable.Set[Global#Symbol]] = new mutable.HashMap

  def removeChildOverride(child: Global#Symbol): Option[mutable.Set[Global#Symbol]] = {
    remainingChildOverrides.remove(child)
  }

}

sealed abstract class ModelField extends ModelSymbol {

  val fields: Option[ModelFields]
  val tree: Global#ValDef
  val isAbstract: Boolean

  //for a class val, this is the associated ctor field if it exists
  var constructorParam = Option.empty[ModelCommon]
  def addConstructorParam(constructorParam:ModelCommon) = {
    assert (this.constructorParam.isEmpty)
    assert (constructorParam != null)
    this.constructorParam = Some(constructorParam)
  }
  //for a method param val, this is the associated default accessor if it exists
  var defaultGetter = Option.empty[ModelCommon]
  def addDefaultGetter(defaultGetter: ModelCommon) = {
    assert (this.defaultGetter.isEmpty)
    assert (defaultGetter != null)
    assert (defaultGetter != null)
    this.defaultGetter = Some(defaultGetter)
  }

}

case class ModelCommon(
    isGlobal: Boolean,
    elementId: ElementId,
    sourceFile: String,
    posStart: Int,
    posEnd: Int,
    posFocus: Int,
    sourceName: String
) extends HasModelCommon {
  override def common: ModelCommon = this

  //TODO remove isGlobal and sourceName as they should come from newId

  override def newCsvString: String = elementId.id
}

case class ModelFields(tree: Global#ValDef, common: ModelCommon, isLazy: Boolean) extends ModelSymbol {
  //TODO should validate that there is some reference, but the references may not match `fieldCount`, but can't exceed it
  //TODO should validate that fields are consistent
  private var fields = List.empty[ModelField]

  def addField(field: ModelField): Unit = {
    fields ::= field
  }

  def syntheticName: Global#TermName = tree.name
  def fieldCount: Int                = tree.tpe.typeArgs.size
}

case class ModelVar(
    tree: Global#ValDef,
    common: ModelCommon,
    isAbstract: Boolean,
    override val isParameter: Boolean,
    fields: Option[ModelFields]
) extends ModelField

case class ModelVal(
    tree: Global#ValDef,
    common: ModelCommon,
    isAbstract: Boolean,
    isLazy: Boolean,
    override val isParameter: Boolean,
    fields: Option[ModelFields]
) extends ModelField

sealed trait ModelMethod extends ModelSymbol {
  val tree: Global#DefDef
  val common: ModelCommon
  val isTyped: Boolean
  val isAbstract: Boolean
}

sealed trait ModelAccessorMethod extends ModelMethod {
  var addedAccessor = false
}

case class ModelGetterMethod(tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean)
    extends ModelAccessorMethod

case class ModelSetterMethod(tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean)
    extends ModelAccessorMethod

case class ModelPlainMethod(tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean)
    extends ModelMethod

case class ModelObject(tree: Global#ModuleDef, common: ModelCommon) extends ModelSymbol with ClassLike

case class ModelClass(tree: Global#ClassDef, common: ModelCommon, isAbstract: Boolean)
    extends ModelSymbol
    with ClassLike

case class ModelTrait(tree: Global#ClassDef, common: ModelCommon) extends ModelSymbol with ClassLike
case class ModelSource(tree: Global#Tree, common: ModelCommon)    extends ModelSymbol
