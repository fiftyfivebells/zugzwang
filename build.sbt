inThisBuild(
  List(
    organization      := "com.ffb",
    scalaVersion      := "3.7.2",
    semanticdbEnabled := true,
    salacOptions ++= Seq("-deprecation", "-Wunused:imports")
  )
)

lazy val root = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    semanticdbEnabled := true,
    scalafmtOnCompile := true,
    scalafixOnCompile := true
  )
  .settings(
    name                                            := "zugzwang",
    version                                         := "0.1.0",
    organization                                    := "com.ffb",
    description                                     := "Pure chess rules logic in Scala 3; every move is legal, but none are good.",
    libraryDependencies += "org.scalatest"          %% "scalatest"                  % "3.2.16" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )

addCommandAlias("lint", "; scalafmtAll; scalafixAll")
