package scalaclean.model.impl.v2

import java.nio.file.{Files, Path}

import scalaclean.model.impl._


object ModelReader {

  def read(project: Project, path: Path, projectRoot: Path) : (Vector[ElementModelImpl], BasicRelationshipInfo) = {

    val refersToB = List.newBuilder[RefersImpl]
    val extendsB = List.newBuilder[ExtendsImpl]
    val overridesB = List.newBuilder[OverridesImpl]
    val withinB = List.newBuilder[WithinImpl]

//    val dir = projectRoot.resolve(path).toAbsolutePath
    val dir = projectRoot.resolve(".").toAbsolutePath
    val relsPath = dir.resolve(IoTokens.fileRelationships)
    println(s"reading relationships from $relsPath")

    Files.lines(relsPath) forEach {
      line =>
        val tokens = line.split(",")

        val from = SymbolCache(tokens(0))
        val relType = tokens(1)
        val to = SymbolCache(tokens(2))

        relType match {
          case IoTokens.relRefers =>
            val isSynthetic = tokens(3).toBoolean
            refersToB += new RefersImpl(from, to, isSynthetic)
          case IoTokens.relExtends =>
            val isDirect = tokens(3).toBoolean
            extendsB += new ExtendsImpl(from, to, isDirect)
          case IoTokens.relOverrides =>
            val isDirect = tokens(3).toBoolean
            overridesB += new OverridesImpl(from, to, isDirect)
          case IoTokens.relWithin =>
            withinB += new WithinImpl(from, to)
        }
    }
    val refersTo = refersToB.result().groupBy(_.fromSymbol)
    val extends_ = extendsB.result().groupBy(_.fromSymbol)
    val overrides = overridesB.result().groupBy(_.fromSymbol)
    val within = withinB.result().groupBy(_.fromSymbol)

    val relationships = BasicRelationshipInfo(
    refersTo,
    extends_,
    overrides,
    within
    )

    val elePath = dir.resolve(IoTokens.fileElements)
    println(s"reading elements from $elePath")

    val builder = Vector.newBuilder[ElementModelImpl]
    Files.lines(elePath) forEach {
      line =>
        val tokens = line.split(",")

        val typeId = tokens(0)
        val symbol = SymbolCache(tokens(1))
        val src = project.source(tokens(2))
        val start = tokens(3).toInt
        val end = tokens(4).toInt

        val basicInfo = BasicElementInfo(symbol,src,start,end)

        val idx = 5
        val ele:ElementModelImpl = typeId match {
          case IoTokens.typeObject =>
            new ObjectModelImpl(basicInfo, relationships)
          case IoTokens.typeTrait =>
            new TraitModelImpl(basicInfo, relationships)
          case IoTokens.typeClass =>
            new ClassModelImpl(basicInfo, relationships)
          case IoTokens.typeVal =>
            val isAbstract = tokens(idx).toBoolean
            val valName = tokens(idx+1).intern()
            val isLazy = tokens(idx+2).toBoolean
            new ValModelImpl(basicInfo, relationships, valName, isAbstract, isLazy)
          case IoTokens.typeVar=>
            val isAbstract = tokens(idx).toBoolean
            val varName = tokens(idx+1).intern()
            new VarModelImpl(basicInfo, relationships, varName, isAbstract)
          case IoTokens.typeMethod =>
            val isAbstract = tokens(idx).toBoolean
            val methodName = tokens(idx+1).intern()
            val hasDeclaredType = tokens(idx+2).toBoolean
            new MethodModelImpl(basicInfo, relationships, methodName, isAbstract, hasDeclaredType)
          case other =>
            throw new IllegalArgumentException("other")
        }
        builder += ele
    }
    (builder.result(), relationships)

  }


}
