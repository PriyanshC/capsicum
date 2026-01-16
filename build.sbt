scalaVersion := "3.8.0-RC6"

libraryDependencies += "io.github.marcinzh" %% "turbolift-core" % "0.124.0"

val kyoVersion = "1.0-RC1"
libraryDependencies += "io.getkyo" %% "kyo-prelude" % kyoVersion
libraryDependencies += "io.getkyo" %% "kyo-core"    % kyoVersion