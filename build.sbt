import com.typesafe.startscript.StartScriptPlugin

seq(StartScriptPlugin.startScriptForClassesSettings: _*)

name := "slackey"

version := "1.0"

scalaVersion := "2.9.2"

resolvers += "twitter-repo" at "http://maven.twttr.com"

ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

libraryDependencies ++= Seq(
"com.twitter" % "finagle-core" % "1.9.0",
"com.twitter" % "finagle-http" % "1.9.0",
"org.scalaj" %% "scalaj-http" % "0.3.14",
"org.mongodb" %% "casbah" % "2.6.5")
