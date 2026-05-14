ThisBuild / scalaVersion := "3.8.3"

ThisBuild / javacOptions ++= Seq("-source", "21", "-target", "21")

ThisBuild / scalacOptions ++= {
  if (sys.env.contains("CC_DEBUG")) Seq("-Ycc-debug") else Seq.empty
} ++ Seq("-explain", "-Wimplausible-patterns", "-release", "21")


lazy val root = (project in file("."))
  .settings(
    name := "capsicum"
  )

val kyoVersion = "1.0-RC1"
lazy val experiments = (project in file("experiments"))
  .dependsOn(root)
  .settings(
    name := "experiments",
    libraryDependencies ++= Seq(
      "io.github.marcinzh" %% "turbolift-core" % "0.124.0",
      "io.getkyo" %% "kyo-prelude" % kyoVersion,
      "io.getkyo" %% "kyo-core"    % kyoVersion
    )
  )
