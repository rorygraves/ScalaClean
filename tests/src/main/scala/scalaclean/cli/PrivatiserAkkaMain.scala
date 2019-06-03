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
  val akkaWorkspace = toPlatform("../akka/")
  val ivyDir = toPlatform("$HOME$/.ivy2/cache")
  val storagePath = toPlatform("$HOME$/Downloads/temp3")

  val outputClassDir: String = toPlatform(s"$akkaWorkspace/akka-actor/target/classes/")
  val inputClasspath = Classpath(toPlatform(s"$outputClassDir|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/resources.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/rt.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/sunrsasign.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/jsse.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/jce.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/charsets.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/jfr.jar|/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/classes|/Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar|/workspace/akka/akka-actor/target/classes|/Users/rorygraves/.ivy2/cache/com.typesafe/config/bundles/config-1.3.3.jar|/Users/rorygraves/.ivy2/cache/org.scala-lang.modules/scala-java8-compat_2.12/bundles/scala-java8-compat_2.12-0.8.0.jar"))
  val sourceRoot = AbsolutePath(akkaWorkspace)
  val inputSourceDirectories: List[AbsolutePath] = Classpath(toPlatform(s"$akkaWorkspace/akka-actor/src/main/scala")).entries

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
    val targetFiles: List[RelativePath] = FileIO.listAllFilesRecursively(sd).filter(f => f.isFile  && f.toFile.getAbsolutePath.endsWith(".scala")).map(_.toRelative(sd)).toList.sortBy(_.toString())


    val filteredFiles = targetFiles filter {
      case RelativePath(nio) =>
        val path = nio.toString
        ! path.endsWith("akka/dispatch/Mailbox.scala") && // illegal cyclic reference involving class AbstractNodeQueue
        ! path.endsWith("akka/dispatch/Mailboxes.scala") && // no symbol found for: 121: val hasMailboxRequirement: Boolean = mailboxRequirement != classOf[MessageQueue]
        ! path.endsWith("akka/dispatch/ThreadPoolBuilder.scala") && // MissingRequirementError: class akka.dispatch.ThreadPoolConfig.ThreadPoolExecutorServiceFactory in JavaMirror
        ! path.endsWith("akka/event/DeadLetterListener.scala") && // no symbol found for: 32: val done = maxCount != Int.MaxValue && count >= maxCount
        ! path.endsWith("akka/event/EventBus.scala") && // MissingRequirementError: class akka.event.ManagedActorClassification.ManagedActorClassificationMappings in JavaMirror
        ! path.endsWith("akka/event/Logging.scala") && // no symbol found for: 75: val level = levelFor(config.StdoutLogLevel) getOrElse {
        ! path.endsWith("akka/event/LoggingReceive.scala") && // could not match the method
        ! path.endsWith("akka/event/LoggingReceive.scala") && // could not match the method
        ! path.endsWith("akka/io/Dns.scala") && // MissingRequirementError: class akka.io.DnsExt.Settings in JavaMirror
        ! path.endsWith("akka/io/SelectionHandler.scala") && // no symbol found for: 35: val SelectorAssociationRetries: Int
        ! path.endsWith("akka/io/SimpleDnsManager.scala") && // assertion failed: no symbol found for: 22: private val inetDnsEnabled = ext.provider.actorClass
        ! path.endsWith("akka/io/Tcp.scala") && // no symbol found for: 572: val NrOfSelectors: Int =
        ! path.endsWith("akka/io/TcpConnection.scala") && // no symbol found for: 377: val interestedInClose: Set[ActorRef] =
        ! path.endsWith("akka/io/Udp.scala") && //  no symbol found for: 192: val NrOfSelectors: Int
        ! path.endsWith("akka/io/dns/DnsResourceRecords.scala") && // no symbol found for: 92: val priority = it.getShort.toInt & 0xFFFF
        ! path.endsWith("akka/io/dns/internal/DomainName.scala") && // no symbol found for: 41: val offset =...
        ! path.endsWith("akka/io/dns/internal/TcpDnsClient.scala") && // no symbol found for: 52: val data = buffer ++ newData
        ! path.endsWith("akka/pattern/AskSupport.scala") && // could not match the method private[pattern] def $qmark$extension(
          ! path.endsWith("akka/pattern/BackoffOnRestartSupervisor.scala") && // no symbol found for: 51: val nextRestartCount ...
          ! path.endsWith("akka/pattern/BackoffSupervisor.scala") && // no symbol found for: 271: val rnd = 1.0 + T
          ! path.endsWith("akka/pattern/CircuitBreaker.scala") && // could not match the method def onOpen(callback: ⇒ Unit): ...
          ! path.endsWith("akka/pattern/PipeToSupport.scala") && // MissingRequirementError: class akka.pattern.PipeToSupport.PipeableFuture in JavaMirror
          ! path.endsWith("Reflect.scala") && // no symbol
        ! path.endsWith("ActorPath.scala") && // no symbol
          ! path.endsWith("ActorRef.scala") && // no symbol
          ! path.endsWith("ActorSystem.scala") && // no symbol
          ! path.endsWith("CoordinatedShutdown.scala") && // no symbol
          ! path.endsWith("Children.scala") && // no symbol
          ! path.endsWith("ChildrenContainer.scala") && // no symbol
          ! path.endsWith("DeathWatch.scala") && // no symbol
          ! path.endsWith("FaultHandling.scala") && // no symbol
          ! path.endsWith("LightArrayRevolverScheduler.scala") && // no symbol
          ! path.endsWith("AbstractDispatcher.scala") && // no symbol
          ! path.endsWith("AffinityPool.scala") && // no symbol
          ! path.endsWith("Future.scala") && // no symbol
          ! path.endsWith("ActorRefProvider.scala") && // cant find method  - recordOverrides
          ! path.endsWith("DynamicAccess.scala") && // cant find method  - recordOverrides
          ! path.endsWith("Props.scala") && // cant find method  - recordOverrides
          ! path.endsWith("ActorSystemSetup.scala") && // cant find method  - recordOverrides
          ! path.endsWith("TypedActor.scala") && // cant find method  - recordOverrides
          ! path.endsWith("Creators.scala") && // recordOverrides  --- build issue ? MissingRequirementError:
          ! path.endsWith("Inbox.scala") && // recordOverrides  --- build issue ? MissingRequirementError:
          ! path.endsWith("FSM.scala") && // recordOverrides  --- build issue ? MissingRequirementError:
          ! path.endsWith("Dispatcher.scala") && // recordOverrides  --- build issue ? MissingRequirementError:
          ! path.endsWith("BalancingDispatcher.scala") && // recordOverrides  --- build issue ? MissingRequirementError:
          ! path.endsWith("BatchingExecutor.scala") && // recordOverrides  --- build issue ? MissingRequirementError:
          ! path.endsWith("ForkJoinExecutorConfigurator.scala") && // recordOverrides  --- build issue ? MissingRequirementError:
          ! path.endsWith("CachingConfig.scala") && // recordOverrides  --- unsafe issue
      true
    }
    AnalysisHelper.runAnalysis(projectName, inputClasspath, sourceRoot,  inputSourceDirectories, outputClassDir, storagePath, filteredFiles)
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