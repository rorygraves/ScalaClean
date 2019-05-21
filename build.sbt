import sbt.Keys.libraryDependencies

lazy val V = _root_.scalafix.sbt.BuildInfo
inThisBuild(
  List(
    organization := "com.example",
    homepage := Some(url("https://github.com/com/example")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "example-username",
        "Example Full Name",
        "example@email.com",
        url("https://example.com")
      )
    ),
    scalaVersion := V.scala212,
    scalacOptions ++= List(
      "-Yrangepos"
    )
  )
)

skip in publish := true

lazy val rules = project.settings(
  moduleName := "scalafix",
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion
)

lazy val command = project
  .dependsOn(rules, tests)
  .settings(
    moduleName := "command",
    libraryDependencies += "args4j" % "args4j" % "2.0.23", 
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit_2.12.8" % V.scalafixVersion,
      //libraryDependencies += "ch.epfl.scala" %% "scalafix-testkit" % V.scalafixVersion,

    compile.in(Compile) :=
      compile.in(Compile).dependsOn(compile.in(privatiserTestInput, Compile)).value,

)

lazy val input = project.settings(
  addCompilerPlugin(scalafixSemanticdb),
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27",
  scalacOptions += "-P:semanticdb:synthetics:on",
  skip in publish := true
)

lazy val privatiserTestInput = project.in(file("privatiser-test-input")).settings(
  addCompilerPlugin(scalafixSemanticdb),
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27",
  scalacOptions += "-P:semanticdb:synthetics:on",
  skip in publish := true
)

lazy val output = project.settings(
  skip in publish := true
)

lazy val tests = project
  .settings(
    skip in publish := true,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    compile.in(Compile) := 
      compile.in(Compile).dependsOn(compile.in(input, Compile)).value,
    scalafixTestkitOutputSourceDirectories :=
      sourceDirectories.in(output, Compile).value,
    scalafixTestkitInputSourceDirectories :=
      sourceDirectories.in(input, Compile).value,
    scalafixTestkitInputClasspath :=
      fullClasspath.in(input, Compile).value,
  )
  .dependsOn(rules)
  .enablePlugins(ScalafixTestkitPlugin)
