ThisBuild / scalaVersion     := "3.3.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.secretsanta"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "SecretSanta",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.19",
      "dev.zio" %% "zio-direct" % "1.0.0-RC7" % Test,
      "dev.zio" %% "zio-test" % "2.0.19" % Test,
      "com.sun.mail" % "javax.mail" % "1.6.2",
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
