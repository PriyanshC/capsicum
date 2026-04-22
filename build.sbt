scalaVersion := "3.8.3"

libraryDependencies += "io.github.marcinzh" %% "turbolift-core" % "0.124.0"

val kyoVersion = "1.0-RC1"
libraryDependencies += "io.getkyo" %% "kyo-prelude" % kyoVersion
libraryDependencies += "io.getkyo" %% "kyo-core"    % kyoVersion

scalacOptions ++= {
  if (sys.env.contains("DEBUG") || true)
    Seq("-Ycc-debug")
  else
    Seq.empty
}
