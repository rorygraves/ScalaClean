import sbt.Keys.libraryDependencies

lazy val V = _root_.scalafix.sbt.BuildInfo
inThisBuild(
  List(
    organization := "scalaclean",
    homepage := Some(url("https://github.com/rorygraves")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion := "2.12.9",
    scalacOptions ++= List(
      "-Yrangepos"
    )
  )
)

lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.example",
  scalaVersion := "2.10.1",
  test in assembly := {}
)
skip in publish := true

lazy val mergeSettings = Def.settings(
  test.in(assembly) := {},
  logLevel.in(assembly) := Level.Error,
  assemblyJarName.in(assembly) :=
    name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
  assemblyOption.in(assembly) ~= { _.copy(includeScala = false) },
  Keys.`package`.in(Compile) := {
    val slimJar = Keys.`package`.in(Compile).value
    val fatJar =
      new File(crossTarget.value + "/" + assemblyJarName.in(assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), CopyOptions(true,false,false))
    slimJar
  },
  packagedArtifact.in(Compile).in(packageBin) := {
    val temp = packagedArtifact.in(Compile).in(packageBin).value
    val (art, slimJar) = temp
    val fatJar =
      new File(crossTarget.value + "/" + assemblyJarName.in(assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), CopyOptions(true,false,false))
    (art, slimJar)
  },
  assemblyMergeStrategy.in(assembly) := {
    case PathList("com", "sun", _*) => MergeStrategy.discard
    case PathList("sun", _*) => MergeStrategy.discard
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  },
)

lazy val analysisPlugin = project.settings(
    moduleName := "analysisPlugin",
    scalaVersion:= V.scala212,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % V.scala212,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % V.scala212,
    mergeSettings,

    libraryDependencies += "org.scalameta" % "semanticdb-scalac-core_2.12.9" % "4.2.3",

    scalacOptions in Test ++= {
      // we depend on the assembly jar
      val jar = (assembly in Compile).value
      println("JAR = " + jar.getAbsolutePath)
      Seq(
        "-Yrangepos",
        s"-Xplugin:${jar.getAbsolutePath}",
        s"-Jdummy=${jar.lastModified}", // ensures recompile
      )

    },
    fork in Test := true
  )

fork in Test := true

lazy val command = project
  .settings(
    moduleName := "command",
    scalaVersion := V.scala212,
    libraryDependencies += "args4j" % "args4j" % "2.33",
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit_2.12.8" % V.scalafixVersion,
    libraryDependencies += "junit" % "junit" % "4.12" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,

    compile.in(Compile) :=
      compile.in(Compile).dependsOn(compile.in(unitTestProject, Compile), compile.in(privatiserProject1, Compile), compile.in(deadCodeProject1, Compile)).value,

  )


lazy val unitTestProject = project.in(file("testProjects/unitTestProject")).settings(
  addCompilerPlugin(scalafixSemanticdb),
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.28",
  scalacOptions += "-P:semanticdb:synthetics:on",
  skip in publish := true
)

lazy val privatiserProject1 = project.in(file("testProjects/privatiserProject1")).settings(
  addCompilerPlugin(scalafixSemanticdb),
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.28",
  scalacOptions += "-P:semanticdb:synthetics:on",
  skip in publish := true
).dependsOn(analysisPlugin)

// template for dead code projects
def deadCodeProject(id: String, projectLocation: String) = sbt.Project.apply(id, file(projectLocation)).settings(
  addCompilerPlugin(scalafixSemanticdb),
  scalacOptions += "-Yrangepos",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27",
  scalacOptions += "-P:semanticdb:synthetics:on",
  skip in publish := true,

  scalacOptions  ++= {
    // we depend on the assembly jar
    //    val baseDirectory.value /"custom_lib"
    val jar = (assembly in Compile in analysisPlugin).value
    Seq(
      "-Xprint:typer",
//      "-Ycompact-trees",
      "-Yrangepos",
      s"-Xplugin:${jar.getAbsolutePath}",
      s"-Jdummy=${jar.lastModified}", // ensures recompile
    )

  },
).dependsOn(analysisPlugin) // here to ensure rebuild on change


lazy val deadCodeProject1 = deadCodeProject("deadCodeProject1","testProjects/deadCodeProject1")
lazy val deadCodeProject2 = deadCodeProject("deadCodeProject2", "testProjects/deadCodeProject2")
lazy val deadCodeProject3 = deadCodeProject("deadCodeProject3", "testProjects/deadCodeProject3")
lazy val deadCodeProject4Broken = deadCodeProject("deadCodeProject4", "testProjects/deadCodeProject4")

lazy val tests = project.dependsOn(command, unitTestProject, privatiserProject1, deadCodeProject1, deadCodeProject2)
  .settings(
    moduleName := "tests",
    libraryDependencies += "args4j" % "args4j" % "2.0.23",
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit_2.12.8" % V.scalafixVersion,
    libraryDependencies += "junit" % "junit" % "4.12" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    scalaVersion := V.scala212,
    crossPaths := false,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
  )
