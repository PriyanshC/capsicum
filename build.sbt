scalaVersion := "3.8.3"

libraryDependencies += "io.github.marcinzh" %% "turbolift-core" % "0.124.0"

val kyoVersion = "1.0-RC1"
libraryDependencies += "io.getkyo" %% "kyo-prelude" % kyoVersion
libraryDependencies += "io.getkyo" %% "kyo-core"    % kyoVersion

javacOptions ++= Seq("-source", "21", "-target", "21")

scalacOptions ++= {
  if (sys.env.contains("CC_DEBUG")) Seq("-Ycc-debug") else Seq.empty,
}
scalacOptions ++= {
  if (sys.env.contains("DEBUG")) Seq("-explain-types") else Seq.empty,
}

scalacOptions ++= Seq("-release", "21")

