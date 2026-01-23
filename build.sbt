inThisBuild(
  List(
    organization      := "com.ffb",
    scalaVersion      := "3.7.2",
    semanticdbEnabled := true,
    scalacOptions ++= Seq("-deprecation", "-Wunused:imports", "-optimize", "-inline-threshold", "32")
  )
)

lazy val core = (project in file("core"))
  .settings(
    name              := "zugzwang-core",
    scalafmtOnCompile := true,
    scalafixOnCompile := true,
    libraryDependencies ++= (
      Seq(
        "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
        "org.scalatest"          %% "scalatest"                  % "3.2.16" % Test
      )
    )
  )

lazy val engine = (project in file("engine"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(
    name              := "zugzwang-engine",
    scalafmtOnCompile := true,
    scalafixOnCompile := true,
    libraryDependencies ++= (
      Seq(
        "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
      )
    )
  )

lazy val root = (project in file("."))
  .aggregate(core, engine)
  .settings(
    name           := "zugzwang",
    publish / skip := true
  )

addCommandAlias("lint", "; scalafmtAll; scalafixAll")
