package scalaclean.model.impl.v2

import scalaclean.model.ModelKey
import scalafix.v1.Symbol

import scala.meta.Term
import scala.reflect.runtime.JavaUniverse

class ExtraParsedData(parser: ParserImpl) {
  def buildAdditionalData(element: ParsedElement): Unit = {
    element match {
      case v: ParsedVal => recordFieldOverrides(element, "value", v.field.name.value)
      case v: ParsedVar => recordFieldOverrides(element, "variable", v.field.name.value)
      case v: ParsedObjectImpl => recordFieldOverrides(element, "object", v.cls.name.value)
      case m: ParsedMethod => recordMethodOverrides(m)
      case t: ParsedTraitImpl =>
      case t: ParsedClassImpl =>
    }
  }
  import parser.internalAccess.ru
  val globalHelper = parser.internalAccess.globalHelper

  private def reflectSymbol(element: ParsedClassLike) = parser.internalAccess.reflectSymbol(element)

  private def recordFieldOverrides(element: ParsedElement, fieldType: String, name: String) = {

    element.enclosing.headOption match {
      case Some(cls: ParsedClassLike) =>
        val sym = reflectSymbol(cls)
        val wanted = s"$fieldType ${name}"
        val found = sym.toType.decls.toList.filter {
          _.toString == wanted
          //            case sym: JavaUniverse#TermSymbol => sym.keyString == keyString && sym.unexpandedName.getterName.decode.toString == field.name.value
          //            case _ => false
        }
        recordInheritance(element, found)

      case _ => // local cant override

    }
  }
  def recordInheritance(element: ParsedElement, localSymbols: List[ru.Symbol]): Unit = {
    val direct = List.newBuilder[ru.Symbol]
    val all = List.newBuilder[ru.Symbol]
    localSymbols foreach {
      sym =>
        val thisType = sym.owner.thisType
        val builder = List.newBuilder[ru.Symbol]
        for (parent <- sym.owner.ancestors) {
          val parentOveridden = sym.matchingSymbol(parent, thisType)
          if (parentOveridden ne ru.NoSymbol) builder += parentOveridden
        }
        val allParents = builder.result()
        allParents.headOption foreach (direct += _)
        all ++= allParents
    }
    val directSyms = direct.result map (rsym => ModelKey.fromGlobal(Symbol(globalHelper.gSymToMSymString(rsym))))
    val allSyms = all.result map (rsym => ModelKey.fromGlobal(Symbol(globalHelper.gSymToMSymString(rsym))))
    if (directSyms.mkString contains "<no symbol>") {
      println("")
    }
    element.recordOverrides(directSyms, allSyms)
  }

  private def paramsMatchName(gSymParamss: List[List[JavaUniverse#Symbol]], metaParamss: List[List[Term.Param]]): Boolean = {
    gSymParamss.size == metaParamss.size &&
      (gSymParamss zip metaParamss forall {
        case (gSymParams: List[JavaUniverse#Symbol], metaParams: List[Term.Param]) =>
          gSymParams.length == metaParams.length &&
            (gSymParams zip metaParams forall {
              case (gSymParam: JavaUniverse#Symbol, metaParam: Term.Param) =>
                assert(metaParam.decltpe.isDefined)
                val gName = gSymParam.nameString
                val mName = metaParam.name.toString()

                gName == mName
            })
      })
  }
  private def paramsMatchType(gSymParamss: List[List[JavaUniverse#Symbol]], metaParamss: List[List[Term.Param]]): Boolean = {
    gSymParamss.size == metaParamss.size &&
      (gSymParamss zip metaParamss forall {
        case (gSymParams: List[JavaUniverse#Symbol], metaParams: List[Term.Param]) =>
          gSymParams.length == metaParams.length &&
            (gSymParams zip metaParams forall {
              case (gSymParam: JavaUniverse#Symbol, metaParam: Term.Param) =>
                assert(metaParam.decltpe.isDefined)
                //TODO not a great compare
                //strip spaces Foo[A, B] -->  Foo[A,B]
                //strip FQ outer names in generic params Foo[a.b.A, c.d.B] --> Foo[A,B]
                //regex
                // 1. capture as $1 a '[' or ','
                // 2. match anything except '[' ']' and ',' followed by a '.'
                // 3. replace with $1

                val gType = gSymParam.tpe.toString.replace(" ", "")replaceAll("([\\,\\[])[^,\\[\\]]+\\.", "$1")
                val mType = metaParam.decltpe.get.toString.replace(" ", "")replaceAll("([\\,\\[])[^,\\[\\]]+\\.", "$1")

                (gType == mType || gType.endsWith(s".$mType"))
            })
      })
  }

  def recordMethodOverrides(m: ParsedMethod): Unit = {
    //record overrides
    //for a method to override it must have a parent which is a class/trait
    //so no locals

    //TODO consider varargs
    //TODO can we put this in scalafix without the hop to reflection

    m.enclosing.headOption match {
      case Some(cls: ParsedClassLike) =>
        val sym = reflectSymbol(cls)
        val list = sym.toType.decls.toList
        val methodName = m.method_name.toString
        val methodParamss = m.method_paramss
        val simpleMethodMatch = list.collect {
          case sym: parser.internalAccess.ru.MethodSymbol
            if !sym.isClassConstructor
              && sym.nameString == methodName => sym
        }
        val foundNamesMatch = simpleMethodMatch filter {
          case sym =>
            val symParams = sym.paramLists
            (symParams, methodParamss) match {
              case (Nil, List(Nil)) => true
              case (List(Nil), Nil) => true
              case _ => paramsMatchName(symParams, methodParamss)
            }
        }
        val found = if (foundNamesMatch.size <= 1) foundNamesMatch else foundNamesMatch filter {
          case sym =>
            val symParams = sym.paramLists
            (symParams, methodParamss) match {
              case (Nil, List(Nil)) => true
              case (List(Nil), Nil) => true
              case _ => paramsMatchType(symParams, methodParamss)
            }
        }
        assert(found.size == 1, s"could not match the method ${m.stat} from $simpleMethodMatch - found=$found - orig = $list")
        recordInheritance(m,found)
      case _ => // local cant override

    }
  }
}
