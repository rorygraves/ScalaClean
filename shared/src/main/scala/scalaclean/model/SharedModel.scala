package scalaclean.model

import scalaclean.model.impl.ElementPath


abstract sealed class NewElementId {
  def innerScopeString: String
  def id: String
  def debugValue: String = id

}
package impl {
  import java.lang
  import java.nio.file.Path
  import java.util.concurrent.atomic.AtomicInteger

  import scala.annotation.tailrec
  import scala.collection.mutable

  object NewElementIdImpl {

//    import java.util.concurrent.ConcurrentHashMap
//
//    private val interned = new ConcurrentHashMap[String, NewElementIdImpl]

    def apply(id: String) = PathNodes.apply(id)// = interned.computeIfAbsent(id, id => new NewElementIdImpl(id.intern()))
  }

  final class NewElementIdImpl private(val id: String) extends NewElementId {
    override def hashCode(): Int = id.hashCode()

    override def toString: String = s"ModelSymbol[$id]"

    override def innerScopeString: String = {
      val start = id.lastIndexOf('/')
      id.substring(start+1, id.indexOf('@', start))
    }
    val option = Some(this)
  }
  object PathNodes {
    def childThis(owner: ElementPath) = apply(s"${owner.id}/this")

    type Sym = scala.reflect.internal.Symbols#Symbol
    type API = scala.reflect.api.Symbols#Symbol

    private val option_ = new mutable.HashMap[String, Option[ElementPath]]
    //all other values are for Some
    option_(null) = None
    option_("") = None

    private val fromString = new mutable.HashMap[String, ElementPath]
    private val fromSymbol = new mutable.HashMap[Sym, ElementPath]

    private val localSymbolNames = mutable.Map[Sym, String]()
    private val idGen = new AtomicInteger()

    private def localId(sym: Sym) = {
      localSymbolNames.getOrElseUpdate(sym, s"##${idGen.incrementAndGet()}")
    }

    private def buildFromSymbol(sym: Sym): ElementPath = {
      @tailrec def interestingOwner(s: Sym):Sym = {
        if (!s.exists || s.isTrait || s.isClass || s.isModuleOrModuleClass || s.isMethod || s.hasPackageFlag || s.isVal || s.isVar) s
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
        localId(sym)
      } else ""
      val encodedName = sym.encodedName
      val name = encodedName+suffix
      sym match {
        case _ if sym.hasPackageFlag =>
          if (encodedName == "<root>" || encodedName == "")
            NodeRoot
          else
            PackagePath(parent, encodedName)
        case _ if sym.isModule =>
          ObjectPath(parent, name)
        case _ if sym.isClass || sym.isTrait =>
          ClassPath(parent, name)
        case _ if sym.isMethod =>
          def paramName(param: Sym) = {
            val fullName = param.info.typeSymbol.fullName
            if (param.typeParams.isEmpty) fullName
            else s"$fullName[${param.typeParams.map { param => param.info.typeSymbol.fullName }.mkString(";")}]"
          }
          val tparamsString =
            if (sym.typeParams.isEmpty) ""
          else s"[${sym.typeParams.map { param => param.info.typeSymbol.fullName }.mkString(";")}]"
          val paramsString = sym.paramss.map { params => params.map(param => paramName(param)).mkString(";") }.mkString("(", "", ")")

          val methodDescriptor = s"${encodedName}$tparamsString$paramsString$suffix"
          MethodPath(parent, methodDescriptor)
        case _ if sym.isVal || sym.isVar =>
          FieldPath(parent, name)
        case _ if !sym.exists =>
          NodeNone
        case _ if sym.isType =>
          TypePath(parent, name)
        case o =>
           throw new IllegalStateException(s"$o  ${o.getClass}")
      }
    }
    private def buildFromString(id: String): ElementPath = {
      if (id.startsWith("S:")) {
        val rest = id.substring(2)
        SourcePath(rest)
      } else {
        val parentIndex = id.lastIndexOf('/')
        val parent: ElementPath = {
          if (parentIndex == -1) NodeRoot else apply(id.substring(0, parentIndex))
        }
        require (parent.canBeParent, s"parent $parent ${parent.getClass} $id")
        if (id.charAt(parentIndex + 2) == ':') {
          def refine(declaredParent: ElementPath, declaredRest: String): (ElementPath, String) = {
            val lastDot = declaredRest.lastIndexOf('.')
            if (lastDot == -1) (declaredParent, declaredRest)
            else {
              assert (declaredParent eq NodeRoot, s"$declaredParent $id")
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
              ObjectPath(realParent, realRest)
            case c if c == ClassPath.nodeType =>
              val (realParent, realRest) = refine(parent, rest)
              ClassPath(realParent, realRest)
            case c if c == PackagePath.nodeType =>
              val (realParent, realRest) = refine(parent, rest)
              PackagePath(realParent, realRest)
            case c if c == MethodPath.nodeType =>
              MethodPath(parent, rest)
            case c if c == FieldPath.nodeType =>
              FieldPath(parent, rest)
            case c if c == TypePath.nodeType =>
              TypePath(parent, rest)
            case _ => ???
          }
        } else {
          val rest = id.substring(parentIndex + 1)
          rest match {
            case "<root>" => NodeRoot
            case "this" =>
              ThisPath(apply(id.substring(0, parentIndex)))
            case _ =>
              throw new IllegalStateException(s"$id $parentIndex $rest")
          }
        }
      }
    }
    def apply(sym: API): ElementPath = apply(sym.asInstanceOf[Sym])
    def apply(sym: Sym): ElementPath = fromSymbol.getOrElseUpdate(sym, buildFromSymbol(sym))
    def applyAndForceField(sym: Sym): ElementPath = {
      apply(sym) match {
        case f: FieldPath => f
        case getter: MethodPath =>
            apply(s"${getter.parent.id}/${FieldPath.nodeType}:${sym.encodedName}")
        case _ => ???
      }
    }
    def apply(id: String): ElementPath = fromString.getOrElseUpdate(id, buildFromString(id))
    def option(id: String): Option[ElementPath] = option_.getOrElseUpdate(id, Some(buildFromString(id)))
    def apply(path: Path): ElementPath = apply(s"${SourcePath.nodeType}:$path")
  }
  sealed trait ElementPath extends NewElementId {
    def canBeParent: Boolean

    def parent: ElementPath
    def isGlobal = parent.isContentGlobal
    def companionOrSelf: ElementPath = this

    def isContentGlobal: Boolean
    def appendPath(sb: java.lang.StringBuilder): Unit
    override def toString = id
  }
  object NodeRoot extends ElementPath {
    override def innerScopeString: String = ???
    override val id: String = "<root>"
    override def parent: ElementPathNode = ???
    override def canBeParent = true
    override def isContentGlobal: Boolean = true
    override def appendPath(sb: java.lang.StringBuilder): Unit = ()
  }
  object NodeNone extends ElementPath {
    override def innerScopeString: String = ???
    override val id: String = "<none>"
    override def parent: ElementPathNode = ???
    override def canBeParent = false
    override def isContentGlobal: Boolean = true
    override def appendPath(sb: java.lang.StringBuilder): Unit = ()
  }
  sealed abstract class ElementPathNode(val parent: ElementPath) extends ElementPath {

    override def id: String = {
      val sb = new java.lang.StringBuilder
      appendPath(sb)
      sb.toString
    }

    def appendPath(sb: java.lang.StringBuilder): Unit = {
      parent.appendPath(sb)
      appendSelf(sb)
    }

    def appendSelf(sb: lang.StringBuilder)
  }
  sealed abstract class BaseElementPathNode(parent: ElementPath) extends ElementPathNode(parent) {
    override def innerScopeString: String = nodeSourceName

    def appendSelf(sb: lang.StringBuilder) = {
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
  sealed abstract class FQPathNode(parent: ElementPath, nodeSourceName: String) extends SimpleElementPathNode(parent, nodeSourceName) {
    override def appendPath(sb: java.lang.StringBuilder): Unit = {
      parent match {
        case p: PackagePath =>
        case NodeRoot =>
        case _ =>
          parent.appendPath(sb)
      }
      appendSelf(sb)
    }
    override def appendSelf(sb: lang.StringBuilder) = {
      if (sb.length > 0) {
        sb.append('/')
      }
      sb.append(nodeType)
      sb.append(':')
      parent match {
        case p: PackagePath =>
          p.appendFQ(sb)
          sb.append(nodeId)
        case NodeRoot =>
        case _ =>
          sb.append(nodeId)
      }
    }
  }
  abstract class SimpleElementPathNode(parent: ElementPath, final val nodeSourceName: String)extends BaseElementPathNode(parent) {
    override final def nodeId: String = nodeSourceName
    override def canBeParent = true
  }
  object ObjectPath{
    def apply(parent: ElementPath, objectName: String) = new ObjectPath(parent, objectName.intern)
   val nodeType= 'O'
  }
  object ClassPath{
    def apply(parent: ElementPath, className: String) = new ClassPath(parent, className.intern)
   val nodeType= 'C'
  }
  object PackagePath{
    def apply(parent: ElementPath, packageName: String) = new PackagePath(parent, packageName.intern)
   val nodeType= 'P'
  }
  object FieldPath{
    def apply(parent: ElementPath, fieldName: String) = new FieldPath(parent, fieldName.intern)
    val nodeType= 'V'
  }
  object MethodPath{
    def apply(parent: ElementPath, methodDescriptor: String) = new MethodPath(parent, methodDescriptor.intern)
    val nodeType= 'M'
  }
  object ThisPath{
    def apply(parent: ElementPath) = new ThisPath(parent)
  }
  object SourcePath{
    def apply(fileName: String) = new SourcePath(fileName)
    val nodeType= 'S'
  }
  object TypePath{
    def apply(parent: ElementPath, typeName: String) = new TypePath(parent, typeName.intern)
    val nodeType= 'T'
  }
  final class ObjectPath private(parent: ElementPath, objectName: String) extends FQPathNode(parent, objectName) {
    override def nodeType = ObjectPath.nodeType
    override def isContentGlobal: Boolean = parent.isContentGlobal
  }
  final class ClassPath private(parent: ElementPath, className: String) extends FQPathNode(parent, className) {
    override def nodeType = ClassPath.nodeType
    override def isContentGlobal: Boolean = parent.isContentGlobal
  }
  final class PackagePath private(parent: ElementPath, packageName: String) extends FQPathNode(parent, packageName) {
    def appendFQ(sb: lang.StringBuilder) :Unit = {
      parent match {
        case NodeRoot =>
        case p: PackagePath =>
          p.appendFQ(sb)
      }
      sb.append(packageName)
      sb.append('.')
    }

    assert((parent eq NodeRoot) || parent.isInstanceOf[PackagePath])
    override def nodeType = PackagePath.nodeType
    override def isContentGlobal: Boolean = true
  }
  final class FieldPath private(parent: ElementPath, fieldName: String) extends SimpleElementPathNode(parent, fieldName) {
    override def nodeType = FieldPath.nodeType
    override def isContentGlobal: Boolean = false
  }
  final class MethodPath private(parent: ElementPath, methodDescriptor: String) extends BaseElementPathNode(parent) {
    override def nodeType = MethodPath.nodeType
    override def isContentGlobal: Boolean = false
    override def nodeId: String = methodDescriptor
    override def nodeSourceName: String = nodeId.substring(0, nodeId.indexOf('('))
    override def canBeParent = true
  }
  final class ThisPath private(parent: ElementPath) extends ElementPathNode(parent) {
    override def isContentGlobal: Boolean = parent.isContentGlobal

    override def appendSelf(sb: lang.StringBuilder): Unit = sb.append("this")
    override def innerScopeString: String = "this"
    override def canBeParent = false
  }
  final class SourcePath private(fileName: String) extends ElementPathNode(NodeRoot) {
    override def isContentGlobal: Boolean = true

    override def appendSelf(sb: lang.StringBuilder): Unit = sb.append(s"${SourcePath.nodeType}:$fileName")
    override def innerScopeString: String = ???
    override def canBeParent = false
  }
  final class TypePath private(parent: ElementPath, typeName: String) extends ElementPathNode(parent) {
    override def isContentGlobal: Boolean = false
    override def appendSelf(sb: lang.StringBuilder): Unit = sb.append(s"${TypePath.nodeType}:$typeName")
    override def canBeParent: Boolean = false
    override def innerScopeString: String = ???
  }

}
object XX extends App {
  import scala.reflect._
  import scala.reflect.runtime.{universe => ru}
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]
  def getType[T: ru.TypeTag](obj: T): ru.Symbol = ru.typeOf[T].typeSymbol
  def getMethod[T: ru.TypeTag](obj: T, name:String): ru.Symbol = ru.typeOf[T].decls.head

  var res: ElementPath = impl.PathNodes(getType(classOf[App]))
  println(res.toString)
  res = impl.PathNodes(getMethod(classOf[XX], "foo"))
  println(res.toString)

}
class XX {
  def foo(a:Int) = 1

}
