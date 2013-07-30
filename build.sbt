organization := "com.github.nlim"

name := "async-dynamo-client"

version := "0.0.1"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-Xlint","-deprecation", "-unchecked","-encoding", "utf8")

resolvers ++= Seq(
    Resolver.url("Play", url("http://download.playframework.org/ivy-releases/"))(Resolver.ivyStylePatterns),
        "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/",
          "Typesafe Releases Repository" at "http://repo.typesafe.com/typesafe/releases/"
        )

libraryDependencies ++= Seq(
    "play" %% "play" % Option(System.getenv("PLAY_VERSION")).getOrElse("2.1.0") % "compile"
  )

libraryDependencies +=  "com.amazonaws" % "aws-java-sdk" % "1.5.2"

libraryDependencies += 	"commons-io" % "commons-io" % "2.4"
