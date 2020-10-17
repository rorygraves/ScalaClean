val scala212         = "2.12.12"
val scalametaVersion = "4.3.20"
val scalaFixVersion  = "0.9.21"
val junitVersion     = "4.13"
val scalaTestVersion = "3.2.2"

inThisBuild(
  List(
    organization := "org.scalaclean",
    homepage := Some(url("https://github.com/rorygraves/ScalaClean")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion := scala212,
    scalacOptions ++= List(
      "-deprecation",
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
    IO.copy(
      List(fatJar -> slimJar),
      CopyOptions(overwrite = true, preserveLastModified = false, preserveExecutable = false)
    )
    slimJar
  },
  packagedArtifact.in(Compile).in(packageBin) := {
    val temp           = packagedArtifact.in(Compile).in(packageBin).value
    val (art, slimJar) = temp
    val fatJar =
      new File(crossTarget.value + "/" + assemblyJarName.in(assembly).value)
    val _ = assembly.value
    IO.copy(
      List(fatJar -> slimJar),
      CopyOptions(overwrite = true, preserveLastModified = false, preserveExecutable = false)
    )
    (art, slimJar)
  },
  assemblyMergeStrategy.in(assembly) := {
    case PathList("com", "sun", _*) => MergeStrategy.discard
    case PathList("sun", _*)        => MergeStrategy.discard
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  },
)

lazy val shared = project
  .settings(
    moduleName := "shared",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scala212,
    scalaVersion := scala212
  )

lazy val analysisPlugin = project
  .dependsOn(shared)
  .settings(
    moduleName := "analysisPlugin",
    scalaVersion := scala212,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scala212,
    libraryDependencies += "org.scala-lang" % "scala-reflect"  % scala212,
    mergeSettings,
    fork in Test := true
  )

fork in Test := true

lazy val command = project
  .dependsOn(shared)
  .settings(
    moduleName := "command",
    scalaVersion := scala212,
    libraryDependencies += "args4j"            % "args4j"        % "2.33",
    libraryDependencies += "ch.epfl.scala"    %% "scalafix-core" % scalaFixVersion,
    mainClass in assembly := Some("scalaclean.cli.ScalaCleanMain"),
  )

lazy val unitTestProject = project
  .in(file("testProjects/unitTestProject"))
  .settings(
    skip in publish := true,
    scalacOptions ++= {
      // we depend on the assembly jar
      val jar          = (assembly in Compile in analysisPlugin).value
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
def testInputProject(id: String, projectLocation: String, showTrees: Boolean = false)(
    dependencies: ClasspathDep[ProjectReference]*
) = sbt.Project
  .apply(id, file(projectLocation))
  .settings(
    scalacOptions += "-Yrangepos",
    skip in publish := true,
    scalacOptions ++= {
      // we depend on the assembly jar
      //    val baseDirectory.value /"custom_lib"
      val jar          = (assembly in Compile in analysisPlugin).value
      val srcLocations = (sourceDirectories in Compile).value.mkString(java.io.File.pathSeparator)
      assert(srcLocations.nonEmpty)
      val extras =
        if (showTrees)
          List(
//        "-P:scalaclean-analysis-plugin:debug:true",
//        "-Ybrowse:typer",
            "-Xprint:typer"
          )
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
  )
  .dependsOn(analysisPlugin, command)
  .dependsOn(dependencies: _*) // here to ensure rebuild on change

lazy val deadCodeProject1 = testInputProject("deadCodeProject1", "testProjects/deadCodeProject1")()
lazy val deadCodeProject2 = testInputProject("deadCodeProject2", "testProjects/deadCodeProject2")()
lazy val deadCodeProject3 = testInputProject("deadCodeProject3", "testProjects/deadCodeProject3")()
lazy val deadCodeProject4 = testInputProject("deadCodeProject4", "testProjects/deadCodeProject4")()
lazy val deadCodeProject5 = testInputProject("deadCodeProject5", "testProjects/deadCodeProject5")()

lazy val deadCodeProject6a = testInputProject("deadCodeProject6a", "testProjects/deadCodeProject6a")()
lazy val deadCodeProject6b = testInputProject("deadCodeProject6b", "testProjects/deadCodeProject6b")(deadCodeProject6a)

lazy val deadCodeProject7       = testInputProject("deadCodeProject7", "testProjects/deadCodeProject7")()
lazy val deadCodeProject8       = testInputProject("deadCodeProject8", "testProjects/deadCodeProject8")()
lazy val deadCodeProject9       = testInputProject("deadCodeProject9", "testProjects/deadCodeProject9")()
lazy val deadCodeProject10_vals = testInputProject("deadCodeProject10_vals", "testProjects/deadCodeProject10_vals")()

lazy val deadCodeProject11_constants =
  testInputProject("deadCodeProject11_constants", "testProjects/deadCodeProject11_constants")()

lazy val deadCodeProject12_isolated =
  testInputProject("deadCodeProject12_isolated", "testProjects/deadCodeProject12_isolated")()

lazy val deadCodeProject13 =
  testInputProject("deadCodeProject13", "testProjects/deadCodeProject13")()

lazy val privatiserProject1 = testInputProject("privatiserProject1", "testProjects/privatiserProject1")()
lazy val privatiserProject2 = testInputProject("privatiserProject2", "testProjects/privatiserProject2")()
lazy val privatiserProject3 = testInputProject("privatiserProject3", "testProjects/privatiserProject3")()
lazy val privatiserProject4 = testInputProject("privatiserProject4", "testProjects/privatiserProject4")()
lazy val privatiserProject5 = testInputProject("privatiserProject5", "testProjects/privatiserProject5")()
lazy val privatiserProject6 = testInputProject("privatiserProject6", "testProjects/privatiserProject6")()
lazy val privatiserProject7 = testInputProject("privatiserProject7", "testProjects/privatiserProject7")()

lazy val finaliserProject1 = testInputProject("finaliserProject1", "testProjects/finaliserProject1")()

lazy val scratch = testInputProject("scratch", "testProjects/scratch", showTrees = false)()

lazy val privatiserTests = List(
  privatiserProject1,
  privatiserProject2,
  privatiserProject3,
  privatiserProject4,
  privatiserProject5,
  privatiserProject6,
  privatiserProject7
)

lazy val deadCodeTests = List(
  deadCodeProject1,
  deadCodeProject2,
  deadCodeProject3,
  deadCodeProject4,
  deadCodeProject5,
  deadCodeProject6a,
  deadCodeProject6b,
  deadCodeProject7,
  deadCodeProject8,
  deadCodeProject9,
  deadCodeProject10_vals,
  deadCodeProject11_constants,
  deadCodeProject12_isolated,
  deadCodeProject13
)

lazy val finaliserTests = List(finaliserProject1)

lazy val scratchProjects = List(scratch)

lazy val testDep =
  List(command, unitTestProject) ::: privatiserTests ::: deadCodeTests ::: finaliserTests ::: scratchProjects

lazy val tests = project
  .dependsOn(testDep.map(classpathDependency(_)): _*)
  .settings(
    moduleName := "tests",
    libraryDependencies += "args4j"             % "args4j"        % "2.33",
    libraryDependencies += "ch.epfl.scala"     %% "scalafix-core" % scalaFixVersion,
    libraryDependencies += "junit"              % "junit"         % junitVersion           % Test,
    libraryDependencies += "org.scalatest"     %% "scalatest"     % scalaTestVersion       ,
    libraryDependencies += "org.scalatestplus" %% "junit-4-12"    % s"$scalaTestVersion.0" % Test,
    scalaVersion := scala212,
    crossPaths := false,
    parallelExecution := false,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
  )
