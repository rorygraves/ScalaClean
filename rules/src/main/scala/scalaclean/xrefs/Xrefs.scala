package scalaclean.xrefs

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.Symbol

import scala.collection.immutable.HashMap
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Xrefs private( private val byFrom: Map[Symbol, Vector[Symbol]],
                     private val byTo: Map[Symbol, Vector[Symbol]]
                   ) {

}

object Xrefs {
  def readFromFiles(paths: Path*): Xrefs = {
    import scala.collection.immutable.VectorBuilder
    import scala.concurrent.ExecutionContext.Implicits.global

    def freeze(builder: collection.mutable.HashMap[Symbol, VectorBuilder[Symbol]]): HashMap[Symbol, Vector[Symbol]] = {
      builder.map {
        case (k, v) => k -> v.result
      } (collection.breakOut)
    }

    val data = paths.map {path => Future{
      val byFrom = new collection.mutable.HashMap[Symbol, VectorBuilder[Symbol]]
      val byTo = new collection.mutable.HashMap[Symbol, VectorBuilder[Symbol]]

      Files.lines(path).forEach { line =>
        val idx1 = line.indexOf(';')
        val idx2 = line.indexOf(';', idx1+1)

        val from = SymbolCache(line.substring(0, idx1 - 1))
        val to = SymbolCache(line.substring(idx1, idx2 - 1))

        byFrom.getOrElseUpdate(from, new VectorBuilder[Symbol]) += to
        byTo.getOrElseUpdate(to, new VectorBuilder[Symbol]) += from
      }
      (byFrom, byTo)
    }}
//    var byFrom = HashMap.empty[Symbol, Vector[Symbol]]
//    var byTo = HashMap.empty[Symbol, Vector[Symbol]]
//    def add(p1: (Symbol, Vector[Symbol]), p2: (Symbol, Vector[Symbol])) = p1._1 -> (p1._2 ++ p2._2)
//    data.foreach{ r =>
//      val (moreFrom, moreTo) = Await.result(r, Duration.Inf)
//      byFrom = byFrom.merged(moreFrom)(add)
//      byTo = byTo.merged(moreTo)(add)
//    }
//    new Xrefs(byFrom, byTo)
    val byFrom = new collection.mutable.HashMap[Symbol, VectorBuilder[Symbol]]
    val byTo = new collection.mutable.HashMap[Symbol, VectorBuilder[Symbol]]
    data.foreach{ r =>
      val (moreFrom, moreTo) = Await.result(r, Duration.Inf)
      moreFrom.foreach {
        case (k,v) =>
          byFrom.getOrElseUpdate(k, new VectorBuilder[Symbol]) ++= v.result
      }
      moreTo.foreach {
        case (k,v) =>
          byTo.getOrElseUpdate(k, new VectorBuilder[Symbol]) ++= v.result
      }
    }
    new Xrefs(freeze(byFrom), freeze(byTo))
//    for (path <- paths) {
//      val in = Files.newInputStream(path)
//      val reader = new BufferedReader(new InputStreamReader(Files.newInputStream(path)), 64000)
//      var line = reader.readLine()
//      while (line ne null) {
//        val idx1 = line.indexOf(';')
//        val idx2 = line.indexOf(';', idx1+1)
//
//        val from = SymbolCache(line.substring(0, idx1 - 1))
//        val to = SymbolCache(line.substring(idx1, idx2 - 1))
//
//        byFrom.getOrElseUpdate(from, new VectorBuilder[Symbol]) += to
//        byTo.getOrElseUpdate(to, new VectorBuilder[Symbol]) += from
//        line = reader.readLine()
//      }
//    }
//    new Xrefs(byFrom, byTo)
  }
}

object SymbolCache {
  val cache = new ConcurrentHashMap[String, Symbol]()
  def apply(id:String) : Symbol = cache.computeIfAbsent(id, Symbol.apply)
  def apply(id:Symbol) : Symbol = apply(id.value)
}

object Test extends App {
  for (  fileId <- 1 to 10) {
    val out = Files.newBufferedWriter(Paths.get(s"s:\\Symbols$fileId.txt"))
    for (to <- 1 to 1000;
         from <- 1 to 1000;
         prefix <- 1 to 1
    ) {
      out.write(s"${prefix}SOME_SYMBOL_SOME_SYMBOL_$from;${prefix}SOME_SYMBOL_SOME_SYMBOL_$to;")
      out.newLine()
    }
    out.close()
  }

  for (i <- 1 to 10) {
    SymbolCache.cache.clear
    val start = System.currentTimeMillis()
    Xrefs.readFromFiles(1 to 10 map {i => Paths.get(s"s:\\Symbols$i.txt")} : _*)

    val end = System.currentTimeMillis()
    println(s"$i ${end - start}")
  }
}