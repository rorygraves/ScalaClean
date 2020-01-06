import sbt.Keys.libraryDependencies

lazy val scala212 = "2.12.9"
lazy val scalaFixVersion = "0.9.6"

inThisBuild(
  List(
    organization := "org.scalaclean",
    homepage := Some(url("https://github.com/rorygraves/ScalaClean")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion := "2.12.9",
    scalacOptions ++= List(
      "-Yrangepos"
    )
  )
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
    IO.copy(List(fatJar -> slimJar), CopyOptions(overwrite = true,preserveLastModified = false,preserveExecutable = false))
    slimJar
  },
  packagedArtifact.in(Compile).in(packageBin) := {
    val temp = packagedArtifact.in(Compile).in(packageBin).value
    val (art, slimJar) = temp
    val fatJar =
      new File(crossTarget.value + "/" + assemblyJarName.in(assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), CopyOptions(overwrite = true,preserveLastModified = false,preserveExecutable = false))
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

lazy val shared = project
  .settings(
    moduleName := "shared",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scala212,
    scalaVersion := scala212)

lazy val analysisPlugin = project.dependsOn(shared).settings(
    moduleName := "analysisPlugin",
    scalaVersion:= scala212,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scala212,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scala212,
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

lazy val command = project.dependsOn(shared)
  .settings(
    moduleName := "command",
    scalaVersion := scala212,
    libraryDependencies += "args4j" % "args4j" % "2.33",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2",
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % scalaFixVersion,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit_2.12.8" % scalaFixVersion,
    libraryDependencies += "junit" % "junit" % "4.12" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,

    mainClass in assembly := Some("scalaclean.cli.ScalaCleanMain"),
    // exclude some of the semanticdb classes which are imported twice
    assemblyExcludedJars in assembly := {
      val cp = (fullClasspath in assembly).value
      cp.filter { f=>
        f.data.getName.contains("semanticdb-scalac_2.12.8-4.2.1.jar")
      }
    }
  )


lazy val unitTestProject = project.in(file("testProjects/unitTestProject")).settings(
  addCompilerPlugin("org.scalameta" % "semanticdb-scalac_2.12.9" % "4.2.3"),
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.28",
  scalacOptions += "-P:semanticdb:synthetics:on",
  skip in publish := true,

    scalacOptions  ++= {
      // we depend on the assembly jar
      //    val baseDirectory.value /"custom_lib"
      val jar = (assembly in Compile in analysisPlugin).value
      val srcLocations = (sourceDirectories in Compile).value.mkString(java.io.File.pathSeparator)
      Seq(
        //  "-Xprint:typer",
        //      "-Ycompact-trees",
        "-Yrangepos",
        s"-Xplugin:${jar.getAbsolutePath}",
        s"-Jdummy=${jar.lastModified}", // ensures recompile
//        "-P:scalaclean-analysis-plugin:debug:true",
        s"-P:scalaclean-analysis-plugin:srcdirs:$srcLocations",
      )
    }
  )


// template for dead code projects
def testInputProject(id: String, projectLocation: String, showTrees: Boolean = false)(dependencies: ClasspathDep[ProjectReference]*) = sbt.Project.apply(id, file(projectLocation)).settings(
  addCompilerPlugin("org.scalameta" % "semanticdb-scalac_2.12.9" % "4.2.3"),
  scalacOptions += "-Yrangepos",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27",
  scalacOptions += "-P:semanticdb:synthetics:on",
  skip in publish := true,

  scalacOptions  ++= {
    // we depend on the assembly jar
    //    val baseDirectory.value /"custom_lib"
    val jar = (assembly in Compile in analysisPlugin).value
    val srcLocations = (sourceDirectories in Compile).value.mkString(java.io.File.pathSeparator)
    assert(srcLocations.nonEmpty)
    val extras = if (showTrees)
      List(
//        "-P:scalaclean-analysis-plugin:debug:true",
//        "-Ybrowse:typer",
        "-Xprint:typer")
    else List[String]()
    List(
//      "-Ycompact-trees",
//      "-Xprint:all",
      "-Yrangepos",
      s"-Xplugin:${jar.getAbsolutePath}",
      s"-Jdummy=${jar.lastModified}", // ensures recompile
//       "-P:scalaclean-analysis-plugin:debug:true",
      s"-P:scalaclean-analysis-plugin:srcdirs:$srcLocations",
    ) ::: extras
  },
).dependsOn(analysisPlugin, command).dependsOn(dependencies :_*) // here to ensure rebuild on change


lazy val deadCodeProject1 = testInputProject("deadCodeProject1","testProjects/deadCodeProject1")()
lazy val deadCodeProject2 = testInputProject("deadCodeProject2", "testProjects/deadCodeProject2")()
lazy val deadCodeProject3 = testInputProject("deadCodeProject3", "testProjects/deadCodeProject3")()
lazy val deadCodeProject4 = testInputProject("deadCodeProject4", "testProjects/deadCodeProject4")()
lazy val deadCodeProject5 = testInputProject("deadCodeProject5", "testProjects/deadCodeProject5")()

lazy val deadCodeProject6a = testInputProject("deadCodeProject6a", "testProjects/deadCodeProject6a")()
lazy val deadCodeProject6b = testInputProject("deadCodeProject6b", "testProjects/deadCodeProject6b")(deadCodeProject6a)

lazy val deadCodeProject7 = testInputProject("deadCodeProject7", "testProjects/deadCodeProject7")()
lazy val deadCodeProject8 = testInputProject("deadCodeProject8", "testProjects/deadCodeProject8")()
lazy val deadCodeProject9 = testInputProject("deadCodeProject9", "testProjects/deadCodeProject9")()
lazy val deadCodeProject10_vals = testInputProject("deadCodeProject10_vals", "testProjects/deadCodeProject10-vals")()

lazy val privatiserProject1 = testInputProject("privatiserProject1", "testProjects/privatiserProject1")()
lazy val privatiserProject2 = testInputProject("privatiserProject2", "testProjects/privatiserProject2")()
lazy val privatiserProject3 = testInputProject("privatiserProject3", "testProjects/privatiserProject3")()
lazy val privatiserProject4 = testInputProject("privatiserProject4", "testProjects/privatiserProject4")()
lazy val privatiserProject5 = testInputProject("privatiserProject5", "testProjects/privatiserProject5")()
lazy val privatiserProject6 = testInputProject("privatiserProject6", "testProjects/privatiserProject6")()
lazy val privatiserProject7 = testInputProject("privatiserProject7", "testProjects/privatiserProject7")()

lazy val scratch = testInputProject("scratch", "testProjects/scratch", true)()

lazy val privatiserTests = List(privatiserProject1, privatiserProject2, privatiserProject3,
  privatiserProject4, privatiserProject5, privatiserProject6, privatiserProject7)
lazy val deadCodeTests = List(deadCodeProject1, deadCodeProject2, deadCodeProject3, deadCodeProject4,
  deadCodeProject5, deadCodeProject6a, deadCodeProject6b, deadCodeProject7, deadCodeProject8, deadCodeProject9,
  deadCodeProject10_vals
)
lazy val scratchProjects = List(scratch)
lazy val testDep = List(command, unitTestProject) ::: privatiserTests ::: deadCodeTests ::: scratchProjects

lazy val tests = project.dependsOn(testDep map (classpathDependency(_)) : _*)
  .settings(
    moduleName := "tests",
    libraryDependencies += "args4j" % "args4j" % "2.0.23",
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % scalaFixVersion,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit_2.12.8" % scalaFixVersion,
    libraryDependencies += "junit" % "junit" % "4.12" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    scalaVersion := scala212,
    crossPaths := false,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
  )
