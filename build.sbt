import scala.sys.process
inThisBuild(
  List(
    organization      := "com.ffb",
    version           := "0.1.0",
    scalaVersion      := "3.7.2",
    semanticdbEnabled := true,
    scalacOptions ++= Seq("-deprecation", "-Wunused:imports")
  )
)

lazy val core = (project in file("core"))
  .settings(
    name                                   := "zugzwang-core",
    scalafmtOnCompile                      := true,
    scalafixOnCompile                      := true,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % Test
  )

lazy val engine = (project in file("engine"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin, BuildInfoPlugin)
  .settings(
    name                                            := "zugzwang-engine",
    scalafmtOnCompile                               := true,
    scalafixOnCompile                               := true,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      scalaVersion,
      sbtVersion,
      BuildInfoKey.action("gitCommit") {
        process.Process("git rev-parse --short HEAD").!!.trim
      }
    ),
    buildInfoPackage := "com.ffb.zugzwang",
    buildInfoObject  := "BuildInfo"
  )
  .settings(
    assembly / mainClass       := Some("com.ffb.zugzwang.uci.UciMain"),
    assembly / assemblyJarName := "zugzwang.jar"
  )

lazy val root = (project in file("."))
  .aggregate(core, engine)
  .settings(
    name           := "zugzwang",
    publish / skip := true
  )

addCommandAlias("lint", "; scalafmtAll; scalafixAll")
