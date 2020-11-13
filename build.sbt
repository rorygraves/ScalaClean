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
    fork := true,
    javaOptions ++= Seq("-Xmx32G", "-Xss10M", "-XX:-UseCompressedOops", "-XX:-UseCompressedClassPointers")
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
        "-P:scalaclean-analysis-plugin:copySources:true"
      )
    }
  )

// template for projects running non unit tests
def testInputProject(id: String, projectLocation: String, showTrees: Boolean = false, browseTrees: Boolean = false, debug: Boolean = false, encoding: String = "UTF-8")(
    dependencies: ClasspathDep[ProjectReference]*
) = sbt.Project
  .apply(id, file(projectLocation))
  .settings(
    libraryDependencies += "junit"              % "junit"           % junitVersion           ,
    libraryDependencies += "org.scalatest"     %% "scalatest"       % scalaTestVersion       ,
    libraryDependencies += "org.scalatestplus" %% "junit-4-12"      % s"$scalaTestVersion.0" ,
    libraryDependencies += "com.novocode"       % "junit-interface" % "0.11"                 ,

    scalacOptions += "-Yrangepos",
    skip in publish := true,
    scalacOptions ++= {
      // we depend on the assembly jar
      //    val baseDirectory.value /"custom_lib"
      val jar          = (assembly in Compile in analysisPlugin).value
      val srcLocations = (sourceDirectories in Compile).value.mkString(java.io.File.pathSeparator)
      assert(srcLocations.nonEmpty)
      val extras = List(
        if (showTrees) List("-Xprint:typer") else Nil,
        if (browseTrees) List("-Ybrowse:typer") else Nil,
        if (debug) List("-P:scalaclean-analysis-plugin:debug:true") else Nil
      ).flatten

      List(
        "-encoding", encoding,
        "-Yrangepos",
        s"-Xplugin:${jar.getAbsolutePath}",
        s"-Jdummy=${jar.lastModified}", // ensures recompile
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
lazy val deadCodeProject8       = testInputProject("deadCodeProject8_overrides", "testProjects/deadCodeProject8_overrides")()
lazy val deadCodeProject9       = testInputProject("deadCodeProject9", "testProjects/deadCodeProject9")()
lazy val deadCodeProject10_vals = testInputProject("deadCodeProject10_vals", "testProjects/deadCodeProject10_vals")()

lazy val deadCodeProject11_constants =
  testInputProject("deadCodeProject11_constants", "testProjects/deadCodeProject11_constants")()

lazy val deadCodeProject12_isolated =
  testInputProject("deadCodeProject12_isolated", "testProjects/deadCodeProject12_isolated")()

lazy val deadCodeProject13_case_class =
  testInputProject("deadCodeProject13_case_class", "testProjects/deadCodeProject13_case_class")()

lazy val deadCodeProject14_anon_class =
  testInputProject("deadCodeProject14_anon_class", "testProjects/deadCodeProject14_anon_class")()

lazy val deadCodeProject15_entry_point =
  testInputProject("deadCodeProject15_entry_point", "testProjects/deadCodeProject15_entry_point")()

lazy val deadCodeProject16_params =
  testInputProject("deadCodeProject16_params", "testProjects/deadCodeProject16_params")()

lazy val deadCodeProject17_annotations =
  testInputProject("deadCodeProject17_annotations", "testProjects/deadCodeProject17_annotations")()

lazy val deadCodeProject18_utf8 =
  testInputProject("deadCodeProject18_utf8", "testProjects/deadCodeProject18_utf8", encoding="UTF-8")()

lazy val deadCodeProject18_utf16 =
  testInputProject("deadCodeProject18_utf16", "testProjects/deadCodeProject18_utf16", encoding="UTF-16")()

lazy val deadCodeProject18_utf32 =
  testInputProject("deadCodeProject18_utf32", "testProjects/deadCodeProject18_utf32", encoding="UTF-32")()

lazy val deadCodeProject19_selftype =
  testInputProject("deadCodeProject19_selftype", "testProjects/deadCodeProject19_selftype")()

lazy val privatiserProject1 = testInputProject("privatiserProject1", "testProjects/privatiserProject1")()
lazy val privatiserProject2 = testInputProject("privatiserProject2", "testProjects/privatiserProject2")()
lazy val privatiserProject3 = testInputProject("privatiserProject3", "testProjects/privatiserProject3")()
lazy val privatiserProject4 = testInputProject("privatiserProject4", "testProjects/privatiserProject4")()
lazy val privatiserProject5 = testInputProject("privatiserProject5", "testProjects/privatiserProject5")()
lazy val privatiserProject6 = testInputProject("privatiserProject6", "testProjects/privatiserProject6")()
lazy val privatiserProject7 = testInputProject("privatiserProject7", "testProjects/privatiserProject7")()

lazy val finaliserProject1 = testInputProject("finaliserProject1", "testProjects/finaliserProject1")()

lazy val testScalaPProject = testInputProject("scalap", "testProjects/scalap")()

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
  deadCodeProject13_case_class,
  deadCodeProject14_anon_class,
  deadCodeProject15_entry_point,
  deadCodeProject16_params,
  deadCodeProject17_annotations,
  deadCodeProject18_utf8,
  deadCodeProject18_utf16,
  deadCodeProject18_utf32,
  deadCodeProject19_selftype
)

lazy val otherProjects = List{
  testScalaPProject
}

lazy val finaliserTests = List(finaliserProject1)

lazy val scratchProjects = List(scratch)

lazy val testDep =
  List(command, unitTestProject) ::: privatiserTests ::: deadCodeTests ::: finaliserTests ::: scratchProjects ::: otherProjects

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
