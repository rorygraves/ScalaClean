package scalaclean.model.impl.v2

import java.nio.file.{Files, Path}

import scalaclean.model._
import scalaclean.model.impl._
import scalafix.v1.Symbol


object ModelReader {

  def read(project: Project, path: Path, projectRoot: Path) : (Vector[ElementModelImpl], BasicRelationshipInfo) = {

    val refersToB = List.newBuilder[RefersImpl]
    val extendsB = List.newBuilder[ExtendsImpl]
    val overridesB = List.newBuilder[OverridesImpl]
    val withinB = List.newBuilder[WithinImpl]
    Files.lines(path.resolve(IoTokens.fileRelationships)) forEach {
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


    val builder = Vector.newBuilder[ElementModelImpl]
    Files.lines(path.resolve(IoTokens.fileElements)) forEach {
      line =>
        val tokens = line.split(",")

        val typeId = tokens(0)
        val symbol = SymbolCache(tokens(1))
        val src = project.source(tokens(2))
        val start = tokens(3).toInt
        val end = tokens(4).toInt

        val basicInfo = BasicElementInfo(symbol,src,start,end)

        var idx = 5
        val ele:ElementModelImpl = typeId match {
          case IoTokens.typeObject =>
            new ObjectModelImpl(basicInfo, relationships)
          case IoTokens.typeTrait =>
            new TraitModelImpl(basicInfo, relationships)
          case IoTokens.typeClass =>
            new ClassModelImpl(basicInfo, relationships)
          case IoTokens.typeVal =>
            new ValModelImpl(basicInfo, relationships)
          case IoTokens.typeVar=>
            new VarModelImpl(basicInfo, relationships)
          case IoTokens.typeDef =>
            new MethodModelImpl(basicInfo, relationships)
          case other =>
            throw new IllegalArgumentException("other")
        }
        builder += ele
    }
    (builder.result(), relationships)

  }


}
