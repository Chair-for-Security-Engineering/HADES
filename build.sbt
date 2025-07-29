ThisBuild / scalaVersion := "2.13.10"
ThisBuild / fork := true

val spinalVersion = "1.8.0"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11" % Test
val os = "com.lihaoyi" %% "os-lib" % "0.9.1"
val json = "com.lihaoyi" %% "upickle" % "3.0.0"

lazy val hadesCompilerPlugin = (project in file("hades-compiler-plugin"))
  .settings (
    name := "hades-compiler-plugin",
    organization := "de.rub.hades",
    version := "0.1",
    sbtPlugin := false,
    crossPaths := false,
    exportJars := true,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    Compile / packageBin / artifactPath := baseDirectory.value / "target" / "plugin.jar"
  )

lazy val root = (project in file("."))
 .settings (
   name := "HADES",
   organization := "de.rub.hades",

   // Exclude hades-compiler-plugin directory from compilation
   Compile / unmanagedSourceDirectories := (Compile / unmanagedSourceDirectories).value.filterNot(_.getPath.contains("hades-compiler-plugin")),

   // Add compiler plugin
   scalacOptions += s"-Xplugin:${(hadesCompilerPlugin / Compile / classDirectory).value.getAbsolutePath}",

	 libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin, scalaTest, os, json),
   libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
 ).dependsOn(hadesCompilerPlugin)