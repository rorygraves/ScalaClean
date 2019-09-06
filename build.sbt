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

skip in publish := true

lazy val analysisPlugin = project.settings(
    moduleName := "analysisPlugin",
    scalaVersion:= V.scala212,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % V.scala212,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % V.scala212,
    scalacOptions in Test ++= {
      val jar = (packageBin in Compile).value
      Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}") // ensures recompile
    },
    scalacOptions in Test += "-Yrangepos",
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

lazy val tests = project.dependsOn(command, unitTestProject, privatiserProject1, deadCodeProject1)
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
)

lazy val deadCodeProject1 = project.in(file("testProjects/deadCodeProject1")).settings(
  addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "9.9.9-SNAPSHOT" cross CrossVersion.full),
  scalacOptions += "-Yrangepos",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27",
  scalacOptions += "-P:semanticdb:synthetics:on",
  skip in publish := true
)
