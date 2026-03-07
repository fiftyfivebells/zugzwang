import scala.util.Try
import scala.sys.process

inThisBuild(
  List(
    organization      := "com.ffb",
    version           := "0.8.1",
    scalaVersion      := "3.7.2",
    semanticdbEnabled := true,
    scalacOptions ++= Seq("-deprecation", "-Wunused:imports")
  )
)

lazy val rules = (project in file("rules"))
  .settings(
    name              := "zugzwang-rules",
    scalafmtOnCompile := true,
    scalafixOnCompile := true,
    libraryDependencies ++= Seq(
      "org.scalatest"          %% "scalatest"                 % "3.2.16" % Test,
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    )
  )

lazy val engine = (project in file("engine"))
  .dependsOn(rules)
  .enablePlugins(JmhPlugin, BuildInfoPlugin)
  .settings(
    name              := "zugzwang-engine",
    scalafmtOnCompile := true,
    scalafixOnCompile := true,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    libraryDependencies += "org.scalatest"          %% "scalatest"                  % "3.2.16" % Test,
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
    assembly / mainClass := Some("com.ffb.zugzwang.uci.UciMain"),
    assembly / assemblyMergeStrategy := {
      // engine's Perft shadows rules' Perft — take the engine version
      case PathList("com", "ffb", "zugzwang", "move", n) if n.startsWith("Perft") =>
        MergeStrategy.first
      case x => (assembly / assemblyMergeStrategy).value(x)
    },
    assembly / assemblyJarName := {
      val v        = version.value
      val z        = "zugzwang"
      val isTagged = Try(
        process.Process("git describe --exact-match --tags HEAD").!!.trim.nonEmpty
      ).getOrElse(false)
      if (isTagged) s"$z-$v.jar"
      else {
        val commit = Try(process.Process("git rev-parse --short HEAD").!!.trim).getOrElse("unknown")
        s"$z-$v-dev.$commit.jar"
      }
    }
  )

lazy val root = (project in file("."))
  .aggregate(rules, engine)
  .settings(
    name           := "zugzwang",
    publish / skip := true
  )

addCommandAlias("lint", "; scalafmtAll; scalafixAll")
