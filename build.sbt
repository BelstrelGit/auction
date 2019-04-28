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

/*libs*/
val catsVersion = "1.6.0"
val catsEffectVersion = "1.1.0"

val log4catsVersion = "0.3.0-M2"

val logBackClassicVersion = "1.2.3"

val akkaVersion = "2.5.19"
val akkaActorVersion = "2.5.22"
val akkaHttpVersion = "10.1.7"

val circeVersion = "0.12.0-M1"
val circeDerVersion = "0.12.0-M1"

val nscalaTimeVersion = "2.22.0"

val scalatestVersion = "3.0.5"
val scalacheckVersion = "1.13.4"
val akkaTestkitVersion = "2.5.22"

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % catsEffectVersion

libraryDependencies += "io.chrisdavenport" %% "log4cats-core" % log4catsVersion % Compile withSources() withJavadoc()
libraryDependencies += "io.chrisdavenport" %% "log4cats-slf4j" % log4catsVersion % Compile withSources() withJavadoc()
libraryDependencies += "io.chrisdavenport" %% "log4cats-extras" % log4catsVersion % Compile withSources() withJavadoc()

libraryDependencies += "ch.qos.logback" % "logback-classic" % logBackClassicVersion % Runtime

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaActorVersion % Compile withSources() withJavadoc()
libraryDependencies += "com.typesafe.akka" %% "akka-http" % akkaHttpVersion % Compile withSources() withJavadoc()
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion % Compile withSources() withJavadoc()

libraryDependencies += "io.circe" %% "circe-core" % circeVersion % Compile withSources() withJavadoc()
libraryDependencies += "io.circe" %% "circe-generic" % circeVersion % Compile withSources() withJavadoc()
libraryDependencies += "io.circe" %% "circe-parser" % circeVersion % Compile withSources() withJavadoc()
libraryDependencies += "io.circe" %% "circe-generic-extras" % circeVersion % Compile withSources() withJavadoc()
libraryDependencies += "io.circe" %% "circe-derivation" % circeDerVersion % Compile withSources() withJavadoc()
libraryDependencies += "io.circe" %% "circe-derivation-annotations" % circeDerVersion % Compile withSources() withJavadoc()

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % nscalaTimeVersion % Compile withSources() withJavadoc()

libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % Test withSources() withJavadoc()
libraryDependencies += "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test withSources() withJavadoc()

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % akkaTestkitVersion % Test withSources() withJavadoc()

/*plugins*/
val paradiseVersion = "3.0.0-M11"
val betterMonadicForVersion = "0.3.0"

addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.patch)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForVersion)

