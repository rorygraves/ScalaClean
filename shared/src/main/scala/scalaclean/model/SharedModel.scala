package scalaclean.model

import java.nio.file.Path

import scalaclean.model.impl.PathNodes

import java.lang.{StringBuilder => JStringBuilder}
import scala.reflect.ClassTag

object ElementId {
  val None: ElementId = impl.NodeNone
  val Root: ElementId = impl.NodeRoot

  type Sym = scala.reflect.internal.Symbols#Symbol
  type API = scala.reflect.api.Symbols#Symbol

  def apply(pathType: PathType, name: String): ElementId = PathNodes.apply(pathType, name)

  def apply(sym: API): ElementId   = PathNodes(sym)
  def apply(sym: Sym): ElementId   = PathNodes(sym)
  def apply(path: Path): ElementId = PathNodes(path)

  def applyAndForceField(sym: Sym): ElementId = PathNodes.applyAndForceField(sym)

  def apply(id: String): ElementId = PathNodes(id)

  def option(id: String): Option[ElementId]             = PathNodes.option(id)
  def forClass[T](implicit cls: ClassTag[T]): ElementId = apply(ClassPath, cls.runtimeClass.getName)
  def childThis(elementId: ElementId): ElementId        = PathNodes.childThis(elementId)

}

abstract sealed class ElementId {

  def innerScopeString: String

  def id: String
  def debugValue: String = id
  def testOnlyId: String
  def isThis: Boolean    = false
  def isNone: Boolean    = false
  def isRoot: Boolean    = false
  def isLocal: Boolean

  def parent: ElementId

  def companionOrSelf: ElementId = this

  //implementation detail
  private[model] def canBeParent: Boolean
  private[model] final def isGlobal = parent.isContentGlobal

  private[model] def isContentGlobal: Boolean
  private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder
  override def toString = id
}

sealed trait PathType {
  val nodeType: Char
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

package impl {
  import java.lang
  import java.nio.file.Path
  import java.util.concurrent.atomic.AtomicInteger

  import scala.annotation.tailrec
  import scala.collection.mutable

  private[model] object PathNodes {
    def childThis(owner: ElementId) = apply(s"${owner.id}/this")

    type Sym = scala.reflect.internal.Symbols#Symbol
    type API = scala.reflect.api.Symbols#Symbol

    private val option_ = new mutable.HashMap[String, Option[ElementId]]
    //all other values are for Some
    option_(null) = None
    option_("") = None

    private val fromString = new mutable.HashMap[String, ElementId]
    private val fromSymbol = new mutable.HashMap[Sym, ElementId]

    private val localSymbolNames = mutable.Map[Sym, String]()

    /** parent element -> local generator */
    private val localIdGens = mutable.Map[ElementId, AtomicInteger]()

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
      val name        = encodedName + suffix
      sym match {
        case _ if sym.hasPackageFlag =>
          if (encodedName == "<root>" || encodedName == "")
            NodeRoot
          else
            PackagePathImpl(parent, encodedName)
        case _ if sym.isModule =>
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
            case c if c == TypePathImpl.nodeType =>
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
    def apply(path: Path): ElementId          = apply(s"${SourcePathImpl.nodeType}:$path")
  }

  private[model] object NodeRoot extends ElementId {
    override def isLocal: Boolean                                                                = false
    override def isRoot: Boolean                                                                 = true
    override def innerScopeString: String                                                        = ???
    override val id: String                                                                      = "<root>"
    override def testOnlyId: String                                                              = id
    override def parent: ElementPathNode                                                         = ???
    override private[model] def canBeParent                                                      = true
    override private[model] def isContentGlobal: Boolean                                         = true
    override private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder = sb
  }

  private[model] object NodeNone extends ElementId {
    override def isLocal: Boolean                                                                = false
    override def isNone: Boolean                                                                 = true
    override def innerScopeString: String                                                        = ???
    override val id: String                                                                      = "<none>"
    override def testOnlyId: String                                                              = id
    override def parent: ElementPathNode                                                         = ???
    override private[model] def canBeParent                                                      = false
    override private[model] def isContentGlobal: Boolean                                         = true
    override private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder = sb
  }

  private[model] sealed abstract class ElementPathNode(val parent: ElementId) extends ElementId {

    override lazy val id: String =
      appendPath(new JStringBuilder, false).toString

    override def testOnlyId: String =
      appendPath(new JStringBuilder, true).toString

    override private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder = {
      parent.appendPath(sb, testFormat)
      appendSelf(sb, testFormat)
      sb
    }

    def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit
  }

  private[model] sealed abstract class BaseElementPathNode(parent: ElementId) extends ElementPathNode(parent) {
    override def innerScopeString: String = nodeSourceName

    override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit = {
      if (sb.length > 0) {
        sb.append('/')
      }
      sb.append(nodeType)
      sb.append(':')
      sb.append(nodeId)
    }

    def nodeType: Char
    def nodeId: String
    def nodeSourceName: String
  }

  private[model] sealed abstract class FQPathNode(parent: ElementId, nodeSourceName: String)
      extends SimpleElementPathNode(parent, nodeSourceName) {

    override private[model] def appendPath(sb: JStringBuilder, testFormat: Boolean): JStringBuilder = {
      parent match {
        case p: PackagePathImpl =>
        case NodeRoot           =>
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
      sb.append(nodeType)
      sb.append(':')
      parent match {
        case p: PackagePathImpl =>
          p.appendFQ(sb)
        case _ =>
      }
      sb.append(nodeId)
    }

  }

  private[model] abstract class SimpleElementPathNode(parent: ElementId, final val nodeSourceName: String)
      extends BaseElementPathNode(parent) {
    override def isLocal: Boolean           = nodeSourceName.contains("##")
    override final def nodeId: String       = nodeSourceName
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
    val nodeType                = 'S'
  }

  private[model] object TypePathImpl {
    def apply(parent: ElementId, typeName: String) = new TypePathImpl(parent, typeName.intern)
    val nodeType                                   = 'T'
  }

  final class ObjectPathImpl private (parent: ElementId, objectName: String) extends FQPathNode(parent, objectName) {
    override def nodeType                                = ObjectPath.nodeType
    override private[model] def isContentGlobal: Boolean = parent.isContentGlobal

    override lazy val companionOrSelf = {
      val id = new JStringBuilder(this.id)
      id.setCharAt(id.lastIndexOf("/") + 1, ClassPath.nodeType)
      PathNodes(id.toString())
    }

  }

  final class ClassPathImpl private (parent: ElementId, className: String) extends FQPathNode(parent, className) {
    val creation                                         = new Exception
    override def nodeType                                = ClassPath.nodeType
    override private[model] def isContentGlobal: Boolean = parent.isContentGlobal

    override lazy val companionOrSelf = {
      val id = new JStringBuilder(this.id)
      id.setCharAt(id.lastIndexOf("/") + 1, ObjectPath.nodeType)
      PathNodes(id.toString())
    }

  }

  final class PackagePathImpl private (parent: ElementId, packageName: String) extends FQPathNode(parent, packageName) {

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
    override def nodeType                                = PackagePath.nodeType
    override private[model] def isContentGlobal: Boolean = true
  }

  final class FieldPathImpl private (parent: ElementId, fieldName: String)
      extends SimpleElementPathNode(parent, fieldName) {
    override def nodeType                                = FieldPath.nodeType
    override private[model] def isContentGlobal: Boolean = false
  }

  private[impl] final class MethodPathImpl private (parent: ElementId, methodDescriptor: String)
      extends BaseElementPathNode(parent) {
    override def isLocal: Boolean                        = nodeSourceName.contains("##")
    override def nodeType                                = MethodPath.nodeType
    override private[model] def isContentGlobal: Boolean = false
    override def nodeId: String                          = methodDescriptor
    override def nodeSourceName: String                  = nodeId.substring(0, nodeId.indexOf('('))
    override private[model] def canBeParent              = true
  }

  private[impl] final class ThisPathImpl private (parent: ElementId) extends ElementPathNode(parent) {
    override def isLocal: Boolean = false
    override def isThis: Boolean  = true

    override private[model] def isContentGlobal: Boolean = parent.isContentGlobal

    override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit = sb.append("/this")
    override def innerScopeString: String                 = "this"
    override private[model] def canBeParent               = false
  }

  private[impl] final class SourcePathImpl private (fileName: String) extends ElementPathNode(NodeRoot) {
    override def isLocal: Boolean                        = false
    override private[model] def isContentGlobal: Boolean = true

    override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit = sb.append(s"${SourcePathImpl.nodeType}:$fileName")
    override def innerScopeString: String                 = ???
    override private[model] def canBeParent               = false
  }

  private[impl] final class TypePathImpl private (parent: ElementId, typeName: String) extends ElementPathNode(parent) {
    override def isLocal: Boolean                         = false
    override private[model] def isContentGlobal: Boolean  = false
    override def appendSelf(sb: JStringBuilder, testFormat: Boolean): Unit = sb.append(s"${TypePathImpl.nodeType}:$typeName")
    override private[model] def canBeParent: Boolean      = false
    override def innerScopeString: String                 = ???
  }

}

object XX extends App {
  import scala.reflect._
  import scala.reflect.runtime.{ universe => ru }
  def getTypeTag[T: ru.TypeTag](obj: T)                         = ru.typeTag[T]
  def getType[T: ru.TypeTag](obj: T): ru.Symbol                 = ru.typeOf[T].typeSymbol
  def getMethod[T: ru.TypeTag](obj: T, name: String): ru.Symbol = ru.typeOf[T].decls.head

  var res: ElementId = impl.PathNodes(getType(classOf[App]))
  println(res.toString)
  res = impl.PathNodes(getMethod(classOf[XX], "foo"))
  println(res.toString)

}

class XX {
  def foo(a: Int) = 1

}
