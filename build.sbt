ThisBuild / scalaVersion := "3.8.3"

ThisBuild / javacOptions ++= Seq("-source", "21", "-target", "21")

ThisBuild / scalacOptions ++= {
  if (sys.env.contains("CC_DEBUG")) Seq("-Ycc-debug") else Seq.empty
} ++ Seq("-explain", "-Wimplausible-patterns", "-release", "21", "-experimental")


lazy val root = (project in file("."))
  .settings(
    name := "capsicum",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.19.0" % Test
  )

val kyoVersion = "1.0-RC1"
lazy val experiments = (project in file("experiments"))
  .dependsOn(root)
  .settings(
    name := "experiments",
    libraryDependencies ++= Seq(
      "io.github.marcinzh" %% "turbolift-core" % "0.124.0",
      "io.github.marcinzh" %% "beam-core" % "0.20.0",
      "io.getkyo" %% "kyo-prelude" % kyoVersion,
      "io.getkyo" %% "kyo-core"    % kyoVersion,
      "org.typelevel" %% "cats-core" % "2.13.0",
    )
  )

lazy val benchmarks = (project in file("benchmarks"))
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "capsicum-benchmarks",
    publish / skip := true
  )
