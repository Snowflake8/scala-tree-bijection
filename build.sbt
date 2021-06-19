val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",
    scalacOptions += "-Yretain-trees",

    scalaVersion := scala3Version,
    excludeFilter in unmanagedSources := "Notes.scala",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value
  )
