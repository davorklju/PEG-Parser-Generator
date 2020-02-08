name := "PEG-Parser"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"


addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")