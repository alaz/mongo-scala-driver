import sbt._

class MongoScalaDriverProject(info: ProjectInfo) extends DefaultProject(info) {

    // Versions
    val MongoJavaDriverVersion = "1.4"

    // Dependencies
    val mongoJavaDriver = "org.mongodb" % "mongo-java-driver" % MongoJavaDriverVersion

    val specs = "org.scala-tools.testing" % "specs_2.8.0.RC2" % "1.6.5-SNAPSHOT" % "test"
    val junit = "junit" % "junit" % "4.5" % "test"

    // Repositories
    val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

    // Options
    val extraCompileOptions = Seq("-unchecked", "-deprecation", "-encoding", "utf8")

    // Packaging
    override def packageSrcJar= defaultJarPath("-src.jar")

    // Benchmark action
    val benchmarkConf = config("benchmark")
    def benchmarkClasspath = fullClasspath(benchmarkConf) +++ testClasspath

    def benchmarkTask(args: List[String]) =
        runTask(Some("com.osinka.mongodb.benchmark.overhead"), benchmarkClasspath, args) .
        dependsOn(compile, testCompile) .
        describedAs("Bechmark Scala driver")

    lazy val benchmark = benchmarkTask("50000" :: "4" :: Nil)

    override def compileOptions = super.compileOptions ++ extraCompileOptions.map(x => CompileOption(x))

    override def pomExtra = {
        <inceptionYear>2009</inceptionYear>

        <organization>
            <name>Osinka.com</name>
            <url>http://www.osinka.com</url>
        </organization>
        <licenses>
            <license>
                <name>Apache License, Version 2.0</name>
                <url>http://www.apache.org/licenses/LICENSE-2.0</url>
            </license>
        </licenses>

        <developers>
            <developer>
                <id>alaz</id>
                <email>azarov@osinka.ru</email>
                <name>Alexander Azarov</name>
                <timezone>+3</timezone>
            </developer>
        </developers>

        <mailingLists>
            <mailingList>
                <name>mongo-scala-driver Developer mailing list</name>
                <archive>http://groups.google.com/group/mongodb-scala/topics</archive>
                <post>mongodb-scala@googlegroups.com</post>
                <subscribe>mongodb-scala+subscribe@googlegroups.com</subscribe>
                <unsubscribe>mongodb-scala+unsubscribe@googlegroups.com</unsubscribe>
            </mailingList>
        </mailingLists>

        <issueManagement>
            <system>github</system>
            <url>http://github.com/alaz/mongo-scala-driver/issues</url>
        </issueManagement>
    }
}