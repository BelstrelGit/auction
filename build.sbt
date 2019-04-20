name := "auction"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions := Seq(
  "-encoding",
  "utf8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-target:jvm-1.8",
  "-Ypartial-unification",
  "-language:_",
  "-Xexperimental"
)

val scalatestVersion = "3.0.5"
val scalacheckVersion = "1.13.4"
val akkaActorVersion = "2.5.22"
val akkaTestkitVersion = "2.5.22"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaActorVersion % Compile withSources() withJavadoc()

libraryDependencies += "org.scalatest"  %% "scalatest"   % scalatestVersion % Test withSources() withJavadoc()
libraryDependencies += "org.scalacheck" %% "scalacheck"  % scalacheckVersion % Test withSources() withJavadoc()
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % akkaTestkitVersion  % Test withSources() withJavadoc()

