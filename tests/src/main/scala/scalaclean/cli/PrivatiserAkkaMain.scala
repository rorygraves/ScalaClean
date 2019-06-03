package scalaclean.cli

import java.nio.charset.StandardCharsets

import scalaclean.cli.FileHelper.toPlatform
import scalaclean.rules.AbstractRule
import scalaclean.rules.privatiser.Privatiser
import scalafix.internal.patch.PatchInternals
import scalafix.internal.reflect.ClasspathOps
import scalafix.lint.RuleDiagnostic
import scalafix.rule.RuleName
import scalafix.scalaclean.cli.DocHelper
import scalafix.testkit.DiffAssertions
import scalafix.v1.SemanticDocument

import scala.meta._
import scala.meta.internal.io.FileIO

object PrivatiserAkkaMain {
  def main(args: Array[String]): Unit = {
    new PrivatiserAkkaMain().run
  }
}

class PrivatiserAkkaMain extends DiffAssertions {

  val projectName = "akka-actor"
  val akkaWorkspace = toPlatform("../akka-scalaclean-test/")
  val ivyDir = toPlatform("$HOME$/.ivy2/cache")
  val storagePath = toPlatform("$HOME$/Downloads/temp3")

  val outputClassDir: String = toPlatform(s"$akkaWorkspace/target/scala-2.12/classes/")
  println("OUTPUT CLASS DIR = " + outputClassDir)
  val inputClasspath = Classpath(toPlatform(s"$outputClassDir|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/resources.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/rt.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/sunrsasign.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/jsse.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/jce.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/charsets.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/jfr.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/classes|/Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar|/workspace/akka/akka-actor/target/classes|/Users/rorygraves/.ivy2/cache/com.typesafe/config/bundles/config-1.3.3.jar|/Users/rorygraves/.ivy2/cache/org.scala-lang.modules/scala-java8-compat_2.12/bundles/scala-java8-compat_2.12-0.8.0.jar"))
  val sourceRoot = AbsolutePath(akkaWorkspace)
  val inputSourceDirectories: List[AbsolutePath] = Classpath(toPlatform(s"$akkaWorkspace/src/main/scala")).entries

  def semanticPatch(
                     rule: AbstractRule,
                     sdoc: SemanticDocument,
                     suppress: Boolean
                   ): (String, List[RuleDiagnostic]) = {
    val fixes = Some(RuleName(rule.name) -> rule.fix(sdoc)).map(Map.empty + _).getOrElse(Map.empty)
    PatchInternals.semantic(fixes, sdoc, suppress)
  }

  def run: Unit = {

    val sd = inputSourceDirectories.head
    val targetFiles: List[RelativePath] = FileIO.listAllFilesRecursively(sd).filter(f => f.isFile && f.toFile.getAbsolutePath.endsWith(".scala")).map(_.toRelative(sd)).toList.sortBy(_.toString())


    val filteredFiles = targetFiles filter {
      case RelativePath(nio) =>
        val path = nio.toString.replace('\\', '/')
        println(s"path = $path")

        path != "akka/dispatch/Mailbox.scala" && // illegal cyclic reference involving class AbstractNodeQueue
          path != "akka/util/SerializedSuspendableExecutionContext.scala" && // illegal cyclic reference involving Java module class AbstractNodeQueue
          //
          path != "akka/pattern/BackoffSupervisor.scala" && // assertion failed
          //
          //
          path != "akka/actor/DynamicAccess.scala" && // cant find method  - recordOverrides - implicit parameters
          path != "akka/actor/Props.scala" && // cant find method  - recordOverrides - implicit parameters
          path != "akka/actor/ReflectiveDynamicAccess.scala" && // cant find method  - recordOverrides - implicit parameters
          path != "akka/actor/TypedActor.scala" && // cant find method  - recordOverrides - implicit parameters
          path != "akka/actor/setup/ActorSystemSetup.scala" && // cant find method  - recordOverrides - implicit parameters
          path != "akka/compat/Future.scala" && // cant find method  - recordOverrides - implicit parameters
          path != "akka/event/Logging.scala" && // cant find method  - recordOverrides - implicit parameters
          path != "akka/routing/ConsistentHash.scala" && // cant find method  - recordOverrides - implicit parameters
          //
          path != "akka/pattern/CircuitBreaker.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/pattern/PipeToSupport.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/event/EventBus.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/io/Dns.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/io/SelectionHandler.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/io/Tcp.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/io/TcpConnection.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/dispatch/ThreadPoolBuilder.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/actor/ActorSystem.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/actor/dsl/Creators.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/actor/dsl/Inbox.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/actor/FSM.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/dispatch/BalancingDispatcher.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/dispatch/BatchingExecutor.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/dispatch/Dispatcher.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/dispatch/ForkJoinExecutorConfigurator.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          path != "akka/dispatch/affinity/AffinityPool.scala" && // recordOverrides  --- build issue ? MissingRequirementError:
          //
          path != "akka/dispatch/CachingConfig.scala" && // recordOverrides  --- unsafe issue - unsafe symbol ConfigMemorySize (child of package class config) in runtime reflection universe
          //
          path != "akka/actor/ActorRefProvider.scala" && // recordOverrides  --- UNKNOWN - recordOverrides
          path != "akka/actor/FaultHandling.scala" && // recordOverrides  --- UNKNOWN - makeDecider
          path != "akka/event/LoggingReceive.scala" && // recordOverrides  --- UNKNOWN - create
          path != "akka/japi/JavaAPI.scala" && // recordOverrides  --- UNKNOWN - immutableSeq
          path != "akka/serialization/Serializer.scala" && // recordOverrides  --- UNKNOWN - fromBinary
          //
          path != "akka/util/Index.scala" && // recordOverrides  --- compare -- method missing??
          //
          path != "akka/util/ByteString.scala" && // recordOverrides  --- apply ??? varargs
          //
          path != "akka/util/ByteIterator.scala" && // recordOverrides  --- current - its a @inline private ?????
          //
          path != "akka/util/Helpers.scala" && // recordOverrides  --- requiring - byName ?????
          //
          path != "akka/util/LineNumbers.scala" && // recordOverrides  --- `for` - backticks ?????
          //
          path != "akka/pattern/AskSupport.scala" && //  recordOverrides  --- def $qmark$extension decode ???
          //
          path != "akka/dispatch/Future.scala" && // init - no global
          //
          path != "akka/routing/MurmurHash.scala" && // internal - duplicate symbol on var decl - no global
          true
    }
    AnalysisHelper.runAnalysis(projectName, inputClasspath, sourceRoot, inputSourceDirectories, outputClassDir, storagePath, filteredFiles)
    runPrivatiser(targetFiles)
  }

  def runPrivatiser(targetFiles: Seq[RelativePath]): Unit = {

    val symtab = ClasspathOps.newSymbolTable(inputClasspath)
    val classLoader = ClasspathOps.toClassLoader(inputClasspath)

    println("---------------------------------------------------------------------------------------------------")
    // run privatiser
    val privatiser = new Privatiser()
    privatiser.beforeStart()
    targetFiles.foreach { targetFile =>
      val sdoc = DocHelper.readSemanticDoc(classLoader, symtab, inputSourceDirectories.head, sourceRoot, targetFile)
      val (fixed, messages) = semanticPatch(privatiser, sdoc, suppress = false)

      // compare results
      val tokens = fixed.tokenize.get
      val obtained = tokens.mkString

      val targetOutput = RelativePath(targetFile.toString() + ".expected")
      val outputFile = inputSourceDirectories.head.resolve(targetOutput)
      val expected = FileIO.slurp(outputFile, StandardCharsets.UTF_8)

      val diff = DiffAssertions.compareContents(obtained, expected)
      if (diff.nonEmpty) {
        println("###########> obtained       <###########")
        println(obtained)
        println("###########> expected       <###########")
        println(expected)
        println("###########> Diff       <###########")
        println(error2message(obtained, expected))

        System.out.flush()
        System.exit(1)
      }
    }
  }

}