organization := "me.nsmr"
name := "Nlp100"
version := "1.0"
scalaVersion := "2.10.4"

assemblyMergeStrategy in assembly := {
  case PathList("javax", "servlet", xs @ _*)           => MergeStrategy.first
  case PathList(ps @ _*) if ps.last == "cmap_info.txt" => MergeStrategy.last
  case PathList(ps @ _*) if ps.last endsWith ".xml"    => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".types"  => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".class"  => MergeStrategy.first
  case "application.conf"                              => MergeStrategy.concat
  case "unwanted.txt"                                  => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

libraryDependencies ++= Seq(
	)

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits", "-diagrams")

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

