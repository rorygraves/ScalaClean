package scalaclean.model

import java.nio.file.Path

import java.lang.{ StringBuilder => JStringBuilder }

import scala.reflect.ClassTag

class ElementIdManager {
  private val pathNodes = new impl.PathNodes
  val None: ElementId = pathNodes.NodeNone
  val Root: ElementId = pathNodes.NodeRoot

  type Sym = scala.reflect.internal.Symbols#Symbol
  type API = scala.reflect.api.Symbols#Symbol


  def apply(pathType: PathType, name: String): ElementId = pathNodes.apply(pathType, name)

  def apply(sym: API): ElementId   = pathNodes(sym)
  def apply(sym: Sym): ElementId   = pathNodes(sym)
  def apply(path: Path): ElementId = pathNodes(path)

  def applyAndForceField(sym: Sym): ElementId = pathNodes.applyAndForceField(sym)

  def apply(id: String): ElementId = pathNodes(id)

  def option(id: String): Option[ElementId]             = pathNodes.option(id)
  def forClass[T](implicit cls: ClassTag[T]): ElementId = apply(ClassPath, cls.runtimeClass.getName)
  def childThis(elementId: ElementId): ElementId        = pathNodes.childThis(elementId)

}

abstract sealed class ElementId {

  def innerScopeString: String

  def id: String
  def debugValue: String = id
  def testOnlyId: String
  def pathType: PathType
  final def isThis: Boolean = pathType == ThisPath
  final def isNone: Boolean = pathType == NonePath
  final def isRoot: Boolean = pathType == RootPath
  def dotThis: ElementId
  def isLocal: Boolean = false

  def parent: ElementId
  final def hasParent: Boolean = !isNone && !isRoot

  /**
   * For traits and class - the companion object (even if one doesnt exist in the source)
   * for other types this
   * useful when you want to consider an object/trait/class  by name
   */
  def companionObjectOrSelf: ElementId = this

  /**
   * For traits and class - the companion object (even if one doesnt exist in the source)
   *  For object - the companion class/trait (even if one doesnt exist in the source)
   * for other types this
   */
  def companionOrSelf: ElementId = this

  final def equalsOrHasParent(elementId: ElementId): Boolean = {
    (this eq elementId) || (this.hasParent && parent.equalsOrHasParent(elementId))
  }

  final def equalsOrHasParentScope(elementId: ElementId): Boolean = {
    equalsOrHasParentScopeImpl(elementId.companionObjectOrSelf)
  }

  private def equalsOrHasParentScopeImpl(elementId: ElementId): Boolean = {
    (this.companionObjectOrSelf eq elementId) || (this.hasParent && parent.equalsOrHasParentScopeImpl(elementId))
  }

  //implementation detail
  private[model] def canBeParent: Boolean
  private[model] final def isGlobal = parent.isContentGlobal

  private[model] def isContentGlobal: Boolean
  private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder
  override def toString = id
}

sealed trait PathType {
  def nodeType: Char
}

object ObjectPath extends PathType {
  val nodeType = 'O'
}

object ClassPath extends PathType {
  val nodeType = 'C'
}

object PackagePath extends PathType {
  val nodeType = 'P'
}

object FieldPath extends PathType {
  val nodeType = 'V'
}

object MethodPath extends PathType {
  val nodeType = 'M'
}

object SourcePath extends PathType {
  val nodeType = 'S'
}

object TypePath extends PathType {
  val nodeType = 'T'
}
object ThisPath extends PathType {
  override def nodeType: Char = ???
}
object NonePath extends PathType {
  override def nodeType: Char = ???
}
object RootPath extends PathType {
  override def nodeType: Char = ???
}


package impl {
  import java.lang
  import java.nio.file.Path
  import java.util.concurrent.atomic.AtomicInteger

  import scala.annotation.tailrec
  import scala.collection.mutable

  object PathNodes extends PathNodes
  private[model] class PathNodes {
    def childThis(owner: ElementId) = apply(s"${owner.id}/this")

    type Sym = scala.reflect.internal.Symbols#Symbol
    type API = scala.reflect.api.Symbols#Symbol

    private val fromString = new mutable.HashMap[String, ElementId]
    private val fromSymbol = new mutable.HashMap[Sym, ElementId]
    private val localSymbolNames = new mutable.HashMap[Sym, String]()

    /** parent element -> local generator */
    private val localIdGens = new mutable.HashMap[ElementId, AtomicInteger]()
    private val option_ = new mutable.HashMap[String, Option[ElementId]]
    //all other values are for Some
    option_(null) = None
    option_("") = None

    private def buildFromSymbol(sym: Sym): ElementId = {
      @tailrec def interestingOwner(s: Sym): Sym = {
        if (
          !s.exists || s.isTrait || s.isClass || s.isModuleOrModuleClass || s.isMethod || s.hasPackageFlag || s.isVal || s.isVar
        ) s
        else {
          val o = s.owner
          if (o eq s) throw new IllegalStateException(s"$o  ${o.getClass}")
          interestingOwner(o)
        }
      }

      val parent = {
        val owner = interestingOwner(sym.owner)
        if (!owner.exists) NodeRoot else apply(owner)
      }
      val suffix = if (sym.isLocalToBlock) {
        //for locals we dont have to preserve identity across compiles as they cant be referenced
        //but we need to preserve across the same compile!
        localSymbolNames.getOrElseUpdate(
          sym, {
            val idGen = localIdGens.getOrElseUpdate(parent, new AtomicInteger())
            s"##${idGen.incrementAndGet()}"
          }
        )
      } else ""
      val encodedName = sym.encodedName
      val name = encodedName + suffix
      sym match {
        case _ if sym.hasPackageFlag =>
          if (encodedName == "<root>" || encodedName == "")
            NodeRoot
          else
            PackagePathImpl(parent, encodedName)
        case _ if sym.isModuleOrModuleClass =>
          ObjectPathImpl(parent, name)
        case _ if sym.isClass || sym.isTrait =>
          ClassPathImpl(parent, name)
        case _ if sym.isMethod =>
          def paramName(param: Sym) = {
            val fullName = param.info.typeSymbol.fullName
            if (param.typeParams.isEmpty) fullName
            else s"$fullName[${param.typeParams.map(param => param.info.typeSymbol.fullName).mkString(";")}]"
          }

          val tparamsString =
            if (sym.typeParams.isEmpty) ""
            else s"[${sym.typeParams.map(param => param.info.typeSymbol.fullName).mkString(";")}]"
          val paramsString =
            sym.paramss.map(params => params.map(param => paramName(param)).mkString(";")).mkString("(", "", ")")

          val methodDescriptor = s"${encodedName}$tparamsString$paramsString$suffix"
          MethodPathImpl(parent, methodDescriptor)
        case _ if sym.isVal || sym.isVar =>
          FieldPathImpl(parent, name)
        case _ if !sym.exists =>
          NodeNone
        case _ if sym.isType =>
          TypePathImpl(parent, name)
        case o =>
          throw new IllegalStateException(s"$o  ${o.getClass}")
      }
    }

    private def buildFromString(id: String): ElementId = {
      if (id.startsWith("S:")) {
        val rest = id.substring(2)
        SourcePathImpl(rest)
      } else {
        val parentIndex = id.lastIndexOf('/')
        val parent: ElementId = {
          if (parentIndex == -1) NodeRoot else apply(id.substring(0, parentIndex))
        }
        require(parent.canBeParent, s"parent $parent ${parent.getClass} $id")
        if (id.charAt(parentIndex + 2) == ':') {
          def refine(declaredParent: ElementId, declaredRest: String): (ElementId, String) = {
            val lastDot = declaredRest.lastIndexOf('.')
            if (lastDot == -1) (declaredParent, declaredRest)
            else {
              assert(declaredParent eq NodeRoot, s"$declaredParent $id")
              //the parent must be a package
              val parent = apply(s"${PackagePath.nodeType}:${declaredRest.substring(0, lastDot)}")
              (parent, declaredRest.substring(lastDot + 1))
            }
          }

          // /t:detail
          val rest = id.substring(parentIndex + 3)
          id.charAt(parentIndex + 1) match {
            case c if c == ObjectPath.nodeType =>
              val (realParent, realRest) = refine(parent, rest)
              ObjectPathImpl(realParent, realRest)
            case c if c == ClassPath.nodeType =>
              val (realParent, realRest) = refine(parent, rest)
              ClassPathImpl(realParent, realRest)
            case c if c == PackagePath.nodeType =>
              val (realParent, realRest) = refine(parent, rest)
              PackagePathImpl(realParent, realRest)
            case c if c == MethodPath.nodeType =>
              MethodPathImpl(parent, rest)
            case c if c == FieldPath.nodeType =>
              FieldPathImpl(parent, rest)
            case c if c == TypePath.nodeType =>
              TypePathImpl(parent, rest)
            case _ => ???
          }
        } else {
          val rest = id.substring(parentIndex + 1)
          rest match {
            case "<root>" => NodeRoot
            case "<none>" => NodeNone
            case "this" =>
              ThisPathImpl(apply(id.substring(0, parentIndex)))
            case _ =>
              throw new IllegalStateException(s"$id $parentIndex $rest")
          }
        }
      }
    }

    def apply(sym: API): ElementId = apply(sym.asInstanceOf[Sym])

    def apply(sym: Sym): ElementId = fromSymbol.getOrElseUpdate(
      sym, {
        val raw = buildFromSymbol(sym)
        val res = fromString.getOrElseUpdate(raw.id, raw)
        res
      }
    )

    def applyAndForceField(sym: Sym): ElementId = {
      apply(sym) match {
        case f: FieldPathImpl => f
        case getter: MethodPathImpl =>
          apply(s"${getter.parent.id}/${FieldPath.nodeType}:${sym.encodedName}")
        case _ => ???
      }
    }

    def apply(pathType: PathType, name: String): ElementId = {
      apply(s"${pathType.nodeType}:$name")
    }

    def apply(id: String): ElementId = fromString.getOrElseUpdate(
      id, {
        val res = buildFromString(id)
        assert(res.id == id, s"/n${res.id}/n$id")
        res
      }
    )

    def option(id: String): Option[ElementId] = option_.getOrElseUpdate(id, Some(apply(id)))

    def apply(path: Path): ElementId = apply(s"${SourcePath.nodeType}:$path")


    private[model] object NodeRoot extends ElementId {
      override def innerScopeString: String = ???

      override val id: String = "<root>"

      override def testOnlyId: String = id

      override def parent: ElementPathNode = ???

      override private[model] def canBeParent = true

      override private[model] def isContentGlobal: Boolean = true

      override private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder = sb

      override def dotThis: ElementId = ???

      override def pathType: PathType = RootPath
    }

    private[model] object NodeNone extends ElementId {
      override def innerScopeString: String = ???

      override val id: String = "<none>"

      override def testOnlyId: String = id

      override def parent: ElementPathNode = ???

      override private[model] def canBeParent = false

      override private[model] def isContentGlobal: Boolean = true

      override private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder = sb

      override def dotThis: ElementId = ???

      override def pathType: PathType = NonePath
    }

    private[model] sealed abstract class ElementPathNode(val parent: ElementId) extends ElementId {
      assert (parent.canBeParent)

      override lazy val id: String =
        appendPath(new JStringBuilder, false).toString

      override def testOnlyId: String = appendPath(new JStringBuilder, true).toString

      override private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder = {
        parent.appendPath(sb, testFormat)
        appendSelf(sb, testFormat)
        sb
      }

      def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit

      override lazy val dotThis: ElementId = ThisPathImpl(this)
    }

    private[model] sealed abstract class BaseElementPathNode(parent: ElementId) extends ElementPathNode(parent) {
      override def innerScopeString: String = nodeSourceName

      override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit = {
        if (sb.length > 0) {
          sb.append('/')
        }
        sb.append(pathType.nodeType)
        sb.append(':')
        val localId = nodeId
        if (!testFormat || !isLocal)
          sb.append(localId)
        else
          sb.append(localId.substring(0, localId.indexOf("##") + 2))
      }

      final def nodeType: Char = pathType.nodeType

      def nodeId: String

      def nodeSourceName: String
    }

    private[model] sealed abstract class FQPathNode(parent: ElementId, nodeSourceName: String)
      extends SimpleElementPathNode(parent, nodeSourceName) {

      override private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder = {
        parent match {
          case p: PackagePathImpl =>
          case NodeRoot =>
          case _ =>
            parent.appendPath(sb, testFormat)
        }
        appendSelf(sb, testFormat)
        sb
      }

      override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit = {
        if (sb.length > 0) {
          sb.append('/')
        }
        sb.append(pathType.nodeType)
        sb.append(':')
        parent match {
          case p: PackagePathImpl =>
            p.appendFQ(sb)
          case _ =>
        }
        val localId = nodeId
        if (!testFormat || !isLocal)
          sb.append(localId)
        else
          sb.append(localId.substring(0, localId.indexOf("##") + 2))
      }

    }

    private[model] abstract class SimpleElementPathNode(parent: ElementId, final val nodeSourceName: String)
      extends BaseElementPathNode(parent) {
      override def isLocal: Boolean = nodeSourceName.contains("##")

      override final def nodeId: String = nodeSourceName

      override private[model] def canBeParent = true
    }


    private[model] object ObjectPathImpl {
      def apply(parent: ElementId, objectName: String) = new ObjectPathImpl(parent, objectName.intern)
    }

    private[model] object ClassPathImpl {
      def apply(parent: ElementId, className: String) = new ClassPathImpl(parent, className.intern)
    }

    private[model] object PackagePathImpl {
      def apply(parent: ElementId, packageName: String) = new PackagePathImpl(parent, packageName.intern)
    }

    private[model] object FieldPathImpl {
      def apply(parent: ElementId, fieldName: String) = new FieldPathImpl(parent, fieldName.intern)
    }

    private[model] object MethodPathImpl {
      def apply(parent: ElementId, methodDescriptor: String) = new MethodPathImpl(parent, methodDescriptor.intern)
    }

    private[model] object ThisPathImpl {
      def apply(parent: ElementId) = new ThisPathImpl(parent)
    }

    private[model] object SourcePathImpl {
      def apply(fileName: String) = new SourcePathImpl(fileName)
    }

    private[model] object TypePathImpl {
      def apply(parent: ElementId, typeName: String) = new TypePathImpl(parent, typeName.intern)
    }

    final class ObjectPathImpl private(parent: ElementId, objectName: String) extends FQPathNode(parent, objectName) {
      override def pathType: PathType = ObjectPath

      override private[model] def isContentGlobal: Boolean = parent.isContentGlobal

      override lazy val companionOrSelf = {
        val id = new JStringBuilder(this.id)
        id.setCharAt(id.lastIndexOf("/") + 1, ClassPath.nodeType)
        PathNodes.this.apply(id.toString())
      }

    }

    final class ClassPathImpl private(parent: ElementId, className: String) extends FQPathNode(parent, className) {
      val creation = new Exception

      override def pathType: PathType = ClassPath

      override private[model] def isContentGlobal: Boolean = parent.isContentGlobal

      override lazy val companionObjectOrSelf = {
        val id = new JStringBuilder(this.id)
        id.setCharAt(id.lastIndexOf("/") + 1, ObjectPath.nodeType)
        PathNodes.this.apply(id.toString())
      }

      override def companionOrSelf: ElementId = companionObjectOrSelf
    }

    final class PackagePathImpl private(parent: ElementId, packageName: String) extends FQPathNode(parent, packageName) {

      def appendFQ(sb: JStringBuilder): Unit = {
        parent match {
          case NodeRoot =>
          case p: PackagePathImpl =>
            p.appendFQ(sb)
          case _ => ???
        }
        sb.append(packageName)
        sb.append('.')
      }

      assert(parent.isRoot || parent.isInstanceOf[PackagePathImpl])

      override def pathType: PathType = PackagePath

      override private[model] def isContentGlobal: Boolean = true
    }

    final class FieldPathImpl private(parent: ElementId, fieldName: String)
      extends SimpleElementPathNode(parent, fieldName) {
      override def pathType: PathType = FieldPath

      override private[model] def isContentGlobal: Boolean = false
    }

    private[impl] final class MethodPathImpl private(parent: ElementId, methodDescriptor: String)
      extends BaseElementPathNode(parent) {
      override def isLocal: Boolean = methodDescriptor.contains("##")

      override def pathType: PathType = MethodPath

      override private[model] def isContentGlobal: Boolean = false

      override def nodeId: String = methodDescriptor

      override def nodeSourceName: String = nodeId.substring(0, nodeId.indexOf('('))

      override private[model] def canBeParent = true
    }

    private[impl] final class ThisPathImpl private(parent: ElementId) extends ElementPathNode(parent) {
      assert (!parent.isRoot)
      assert (!parent.isNone)
      assert (!parent.isThis)
      assert (!parent.isLocal)

      override def pathType: PathType = ThisPath

      override private[model] def isContentGlobal: Boolean = parent.isContentGlobal

      override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit = sb.append("/this")

      override def innerScopeString: String = "this"

      override private[model] def canBeParent = false
    }

    private[impl] final class SourcePathImpl private(fileName: String) extends ElementPathNode(NodeRoot) {
      override private[model] def isContentGlobal: Boolean = true

      override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit =
        sb.append(s"${SourcePath.nodeType}:$fileName")

      override def innerScopeString: String = ???

      override private[model] def canBeParent = false

      override def pathType: PathType = SourcePath
    }

    private[impl] final class TypePathImpl private(parent: ElementId, typeName: String) extends ElementPathNode(parent) {
      override private[model] def isContentGlobal: Boolean = false

      override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit =
        sb.append(s"${TypePath.nodeType}:$typeName")

      override private[model] def canBeParent: Boolean = false

      override def innerScopeString: String = ???

      override def pathType: PathType = TypePath
    }

  }

}
