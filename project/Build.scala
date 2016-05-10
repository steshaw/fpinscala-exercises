import sbt._
import Keys._

object FPInScalaBuild extends Build {
  val opts = Project.defaultSettings ++ Seq(
    scalaVersion := "2.11.8",
    scalacOptions := Seq("-unchecked", "-feature", "-deprecation"),
    resolvers ++= Seq(
      Resolver.typesafeRepo("releases"),
      Resolver.sonatypeRepo("releases")
    ),
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.7.1" cross CrossVersion.binary)
  )

  lazy val root =
    Project(id = "fpinscala",
            base = file("."),
            settings = opts ++ Seq(
              onLoadMessage ~= (_ + nio2check())
            )) aggregate (chapterCode, exercises, answers)
  lazy val chapterCode =
    Project(id = "chapter-code",
            base = file("chaptercode"),
            settings = opts)
  lazy val exercises =
    Project(id = "exercises",
            base = file("exercises"),
            settings = opts)
  lazy val answers =
    Project(id = "answers",
            base = file("answers"),
            settings = opts)

  def nio2check(): String = {
    val cls = "java.nio.channels.AsynchronousFileChannel"
    try {Class.forName(cls); ""}
    catch {case _: ClassNotFoundException =>
      ("\nWARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.\n" +
       "You are probably running Java < 1.7; answers will not compile.\n" +
       "You seem to be running " + System.getProperty("java.version") + ".\n" +
       "Try `project exercises' before compile, or upgrading your JDK.")
    }
  }
}
