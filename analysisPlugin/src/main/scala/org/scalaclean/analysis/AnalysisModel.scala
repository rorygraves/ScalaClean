package org.scalaclean.analysis

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.tools.nsc.Global


trait HasModelCommon {
  def common: ModelCommon

  def csvString: String = common.csvString

  def newCsvString: String = common.newCsvString
}

sealed trait ModelSymbol extends HasModelCommon {

  def debugName: String

  val common: ModelCommon
  var traversal: Int = -1

  def isGlobal: Boolean = common.isGlobal

  def semanticRep: String = common.id

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

  def extensionData = _extensionData

  def ioToken: String

  var children = Map[ModelCommon, ModelSymbol]()

  var extendsRels: ListSet[(HasModelCommon, Boolean)] = ListSet.empty
  var overridesRels: ListSet[(HasModelCommon, Boolean)] = ListSet.empty
  var refersRels: ListSet[(HasModelCommon, Boolean)] = ListSet.empty
  var withinRels: ListSet[ModelSymbol] = ListSet.empty
  var gettersFor: ListSet[ModelCommon] = ListSet.empty
  var settersFor: ListSet[ModelCommon] = ListSet.empty

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
    println(newCsvString + " isLocal = " + !isGlobal + ", isParameter= " + isParameter)

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

    children.values.foreach { child =>
      child.printStructureInt(depth + 1)
    }
  }


  def outputStructure(eleWriter: ElementsWriter, relWriter: RelationshipsWriter, extensionWriter: ExtensionWriter): Unit = {
    eleWriter.write(this)
    extensionWriter.writeExtensions(this)

    extendsRels.foreach { case (ext, direct) =>
      relWriter.extendsCls(ext, this, direct)
    }

    overridesRels.foreach { case (ovr, direct) =>
      // bit of a hack with the cast - could be more specfic on subclasses
      relWriter.overrides(this.asInstanceOf[ModelMethod], ovr, direct)
    }

    withinRels.foreach { within =>
      relWriter.within(within, this)
    }

    refersRels.foreach { case (refers, isSynthetic) =>
      relWriter.refers(this, refers, isSynthetic)
    }

    gettersFor.foreach { getterTarget =>
      relWriter.getterFor(this.common, getterTarget)
    }

    settersFor.foreach { setterTarget =>
      relWriter.setterFor(this.common, setterTarget)
    }

    children.values.foreach { child =>
      child.outputStructure(eleWriter, relWriter, extensionWriter)
    }
  }

  def flatten(): Unit = {
    children = children.flatMap { case t@(common, child) =>
      child.flatten()
      child match {
        case mf: ModelField if (mf.isParameter || !mf.isGlobal) =>
          this.refersRels ++= mf.refersRels
          None
        case _ =>
          Some(t)
      }
    }
    refersRels = refersRels.filter(_._1.common.isGlobal)
    refersRels = refersRels.filter(_._1.common.newId != this.common.newId)

  }

}

sealed trait ClassLike extends ModelSymbol {
  private var postProcessing = List.empty[() => Unit]

  def addPostProcess(fn: () => Unit) = postProcessing ::= fn

  def postProcess(): Unit = {
    postProcessing foreach (_.apply())
  }


  /** the overrides that have not yet been processed
    * After the ClassLike is processed this is  the overrides for each of the def/val in the class
    * As they are processed they are removed, and the remainder are processed after the class
    * is completed */
  val remainingChildOverrides: mutable.Map[Global#Symbol, mutable.Set[Global#Symbol]] = new mutable.HashMap

  def removeChildOveride(child: Global#Symbol) = {
    remainingChildOverrides.remove(child)
  }
}

sealed abstract class ModelField extends ModelSymbol {
  val fields: Option[ModelFields]
  val tree: Global#ValDef
}

case class ModelCommon(
                        isGlobal: Boolean, id: String, newId: String, sourceFile: String, posStart: Int, posEnd: Int,posFocus: Int,
                        sourceName: String) extends HasModelCommon {
  override def common: ModelCommon = this

  override def csvString: String = {
    val globalStr = if (isGlobal) "G:" else s"L:$sourceName/"
    s"$globalStr$id"
  }

  override def newCsvString: String = newId
}

case class ModelFields(
                        tree: Global#ValDef, common: ModelCommon, isLazy: Boolean) extends ModelSymbol {
  //TODO should validate that there is some reference, but the references may not match `fieldCount`, but can't exceed it
  //TODO should validate that fields are consistent
  private var fields = List.empty[ModelField]
  def addField(field: ModelField): Unit = {
    fields ::= field
  }

  def syntheticName = tree.name
  def fieldCount = tree.tpe.typeArgs.size
  override def debugName: String = "(compound fields)"

  override def ioToken: String = IoTokens.typeFields
}

case class ModelVar(
                     tree: Global#ValDef, common: ModelCommon, isAbstract: Boolean,
                     override val isParameter: Boolean, fields: Option[ModelFields]) extends ModelField {
  override def debugName: String = "var"

  override def ioToken: String = IoTokens.typeVar
}

case class ModelVal(
                     tree: Global#ValDef, common: ModelCommon, isAbstract: Boolean, isLazy: Boolean,
                     override val isParameter: Boolean, fields: Option[ModelFields]) extends ModelField {
  override def debugName: String = "val"

  override def ioToken: String = IoTokens.typeVal
}

sealed trait ModelMethod extends ModelSymbol {
  val tree: Global#DefDef
  val common: ModelCommon
  val isTyped: Boolean
  val isAbstract: Boolean
}
sealed trait ModelAccessorMethod extends ModelMethod

case class ModelGetterMethod(
                              tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelAccessorMethod {

  override def debugName: String = "getter"

  override def ioToken: String = IoTokens.typeGetterMethod
}

case class ModelSetterMethod(
                              tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelAccessorMethod {
  override def debugName: String = "setter"

  override def ioToken: String = IoTokens.typeSetterMethod
}

case class ModelPlainMethod(
                             tree: Global#DefDef, common: ModelCommon, isTyped: Boolean, isAbstract: Boolean) extends ModelMethod {
  override def debugName: String = "def"

  override def ioToken: String = IoTokens.typePlainMethod
}

case class ModelObject(tree: Global#ModuleDef, common: ModelCommon) extends ModelSymbol with ClassLike {
  override def debugName: String = "object"

  override def ioToken: String = IoTokens.typeObject
}

case class ModelClass(tree: Global#ClassDef, common: ModelCommon, isAbstract: Boolean) extends ModelSymbol with ClassLike {
  override def debugName: String = "class"

  override def ioToken: String = IoTokens.typeClass
}

case class ModelTrait(tree: Global#ClassDef, common: ModelCommon) extends ModelSymbol with ClassLike {
  override def debugName: String = "trait"

  override def ioToken: String = IoTokens.typeTrait
}

case class ModelSource(tree: Global#Tree, common: ModelCommon) extends ModelSymbol {


  override def debugName: String = "source"

  override def ioToken: String = IoTokens.typeSource
}
