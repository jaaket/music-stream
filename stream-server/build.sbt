lazy val root = (project in file("."))
  .settings(
    name := "stream-server",
    scalaVersion := "2.12.5",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.5.11",
      "com.typesafe.akka" %% "akka-stream" % "2.5.11",
      "com.typesafe.akka" %% "akka-http" % "10.1.0",
      "com.amazonaws" % "aws-java-sdk-core" % "1.11.305",
      "com.amazonaws" % "aws-java-sdk-s3" % "1.11.305",
      "ch.megard" %% "akka-http-cors" % "0.3.0",
      "com.typesafe.akka" %% "akka-http-testkit" % "10.1.0" % Test
    )
  )