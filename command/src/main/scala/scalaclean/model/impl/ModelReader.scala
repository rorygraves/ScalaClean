package scalaclean.model.impl

import java.nio.file.{Files, Paths}

import org.scalaclean.analysis.{ExtensionData, ExtensionDescriptor, IoTokens}

import scala.collection.mutable


object ModelReader {
  def read(project: Project, elementsFilePath: String, relationshipsFilePath: String, extensionFilePath: String):
  (Vector[ElementModelImpl], BasicRelationshipInfo) = {


    val relationships = readRels(relationshipsFilePath)
    val (extById, extByNewId) = readExt(extensionFilePath)

    val elements = readElements(project, elementsFilePath, relationships, extById, extByNewId)
    (elements, relationships)
  }

  def finished(): Unit = {
    interner.clear()
    lookup.values.foreach(_.clearData())
    lookup.clear()
  }

  private val interner = mutable.Map.empty[List[ExtensionData], List[ExtensionData]]
  private val lookup = mutable.Map.empty[String, ExtensionDescriptor[_ <: ExtensionData]]

  private def compress(data: List[ExtensionData]): List[ExtensionData] = {
    val sorted = data.sorted
    interner.getOrElseUpdate(sorted, sorted)
  }

  private def readExt(extensionFilePath: String): (Map[String, Seq[ExtensionData]], Map[String, Seq[ExtensionData]]) = {

    import scala.reflect.runtime.universe
    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)

    var mapByLegacyElementId = Map.empty[String, mutable.Builder[ExtensionData, List[ExtensionData]]]
    var mapByElementId = Map.empty[String, mutable.Builder[ExtensionData, List[ExtensionData]]]

    val path = Paths.get(extensionFilePath)
    println(s"reading extensions from $path")

    Files.lines(path) forEach {
      line: String =>
//        println(line)
        val Array(id, newId, fqn, rest) = line.split(",", 4)
        val extBuilder = lookup.getOrElseUpdate(fqn, {
//          println(s"looking up extension $fqn")
          val module = try {
            runtimeMirror.staticModule(fqn)
          } catch {
            case e: Exception =>
              throw new Exception(s"failed to lookup '$fqn' from line '$line'")
          }
          runtimeMirror.reflectModule(module).instance match {
            case valid: ExtensionDescriptor[_] => valid
            case null => throw new IllegalArgumentException("not a valid Extension FQN - expected the name of an object")
            case invalid => throw new IllegalArgumentException(s"not a valid Extension FQN - ${invalid.getClass.getName} is not a ${classOf[ExtensionDescriptor[_]].getName}")
          }
        })
        val ext: ExtensionData = extBuilder.fromCsv(rest)
        val elementValues1 = mapByLegacyElementId.get(id) match {
          case None =>
            val builder = List.newBuilder[ExtensionData]
            mapByLegacyElementId = mapByLegacyElementId.updated(id, builder)
            builder
          case Some(builder) => builder
        }

        val elementValues2 = mapByElementId.get(newId) match {
          case None =>
            val builder = List.newBuilder[ExtensionData]
            mapByElementId = mapByElementId.updated(newId, builder)
            builder
          case Some(builder) => builder
        }

        elementValues1 += ext
        elementValues2 += ext

    }
    (mapByLegacyElementId.map { case (k, b) => k -> compress(b.result) },
      mapByElementId.map { case (k, b) => k -> compress(b.result) })

  }

  private def readRels(relationshipsFilePath: String) = {
    val refersToB = List.newBuilder[RefersImpl]
    val extendsB = List.newBuilder[ExtendsImpl]
    val overridesB = List.newBuilder[OverridesImpl]
    val withinB = List.newBuilder[WithinImpl]
    val getterB = List.newBuilder[GetterImpl]
    val setterB = List.newBuilder[SetterImpl]

    val path = Paths.get(relationshipsFilePath)
    println(s"reading relationships from $path")

    Files.lines(path) forEach {
      line =>
        try {
          val tokens = line.split(",")

          val from = ElementIdImpl(tokens(0))
          val relType = tokens(1)
          val to = ElementIdImpl(tokens(2))

          val offset = 3
          relType match {
            case IoTokens.relRefers =>
              val isSynthetic = tokens(offset).toBoolean
              refersToB += new RefersImpl(from, to, isSynthetic)
            case IoTokens.relExtends =>
              val isDirect = tokens(offset).toBoolean
              extendsB += new ExtendsImpl(from, to, isDirect)
            case IoTokens.relOverrides =>
              val isDirect = tokens(offset).toBoolean
              overridesB += new OverridesImpl(from, to, isDirect)
            case IoTokens.relWithin =>
              withinB += new WithinImpl(from, to)
            case IoTokens.relGetter =>
              getterB += new GetterImpl(from, to)
            case IoTokens.relSetter =>
              setterB += new SetterImpl(from, to)

          }
        } catch {
          case t: Throwable =>
            throw new IllegalStateException(s"Failed to parse line $line", t)
        }
    }
    val refersTo = refersToB.result().groupBy(_.fromElementId)
    val extends_ = extendsB.result().groupBy(_.fromElementId)
    val overrides = overridesB.result().groupBy(_.fromElementId)
    val within = withinB.result().groupBy(_.fromElementId)
    val getter = getterB.result().groupBy(_.fromElementId)
    val setter = setterB.result().groupBy(_.fromElementId)

    BasicRelationshipInfo(
      refersTo,
      extends_,
      overrides,
      within,
      getter,
      setter
    )
  }

  private def readElements(project: Project, elementsFilePath: String, relationships: BasicRelationshipInfo,
                           byId: Map[String, Seq[ExtensionData]], byNewId: Map[String, Seq[ExtensionData]]) = {
    val path = Paths.get(elementsFilePath)
    println(s"reading elements from $path")

    val builder = Vector.newBuilder[ElementModelImpl]
    Files.lines(path) forEach {
      line =>
        try {
          val tokens: Array[String] = if (line.endsWith(",")){
            line.split(",") :+ ""
          } else line.split(",")

          val typeId = tokens(0)
          val symbol = LegacyElementId(tokens(1))
          val modelSymbol = ElementIdImpl(tokens(2))
          val flags = java.lang.Long.parseLong(tokens(3), 16)
          val src = project.source(tokens(4))
          val start = tokens(5).toInt
          val end = tokens(6).toInt
          val traversal = tokens(7).toInt

          val basicInfo = BasicElementInfo(symbol, modelSymbol, src, start, end, flags, byNewId.getOrElse(tokens(2), Nil), traversal)

          val idx = 8
          val ele: ElementModelImpl = typeId match {
            case IoTokens.typeObject =>
              new ObjectModelImpl(basicInfo, relationships)
            case IoTokens.typeTrait =>
              new TraitModelImpl(basicInfo, relationships)
            case IoTokens.typeClass =>
              new ClassModelImpl(basicInfo, relationships)
            case IoTokens.typeFields =>
              val valName = tokens(idx).intern()
              val isLazy = tokens(idx + 1).toBoolean
              val fieldCount = tokens(idx + 2).toInt
              new FieldsModelImpl(basicInfo, relationships, valName, isLazy, fieldCount)
            case IoTokens.typeVal =>
              val isAbstract = tokens(idx).toBoolean
              val valName = tokens(idx + 1).intern()
              val fields = tokens(idx + 2).intern()
              val isLazy = tokens(idx + 3).toBoolean
              new ValModelImpl(basicInfo, relationships, valName, isAbstract, fields, isLazy)
            case IoTokens.typeVar =>
              val isAbstract = tokens(idx).toBoolean
              val varName = tokens(idx + 1).intern()
              val fields = tokens(idx + 2).intern()
              new VarModelImpl(basicInfo, relationships, varName, isAbstract, fields)
            case IoTokens.typePlainMethod =>
              val isAbstract = tokens(idx).toBoolean
              val methodName = tokens(idx + 1).intern()
              val hasDeclaredType = tokens(idx + 2).toBoolean
              new PlainMethodModelImpl(basicInfo, relationships, methodName, isAbstract, hasDeclaredType)
            case IoTokens.typeGetterMethod =>
              val isAbstract = tokens(idx).toBoolean
              val methodName = tokens(idx + 1).intern()
              val hasDeclaredType = tokens(idx + 2).toBoolean
              new GetterMethodModelImpl(basicInfo, relationships, methodName, isAbstract, hasDeclaredType)
            case IoTokens.typeSetterMethod =>
              val isAbstract = tokens(idx).toBoolean
              val methodName = tokens(idx + 1).intern()
              val hasDeclaredType = tokens(idx + 2).toBoolean
              new SetterMethodModelImpl(basicInfo, relationships, methodName, isAbstract, hasDeclaredType)
            case IoTokens.typeSource =>
              new SourceModelImpl(basicInfo, relationships)

            case other =>
              throw new IllegalArgumentException("Unknown token: $other")
          }
          builder += ele
        } catch {
          case t: Throwable =>
            println(s"Failed to parse line: $line")
            throw t
        }
    }
    builder.result()
  }
}
