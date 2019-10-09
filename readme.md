# ScalaClean

ScalaClean is a full program static analysis tool.
By looking at the entire program as a single entity we can do an in-depth analysis that is not 
available when working file by file as most current static analysis tools do.

## Current Status

Runnable, but probably will do something bad ;).
See the instructions below if you want to give it a try.

## Ideas

- Dead code detection removal
- adjust method/class/package visibility to minimum
- find and remove unused parameters (e.g. overriding method but parameter is not used anywhere)
- Many more

## TODO List

- [ ] Run it on more things an fix the bugs

## Running against an external project

We don't currently have a full sbt plugin or publish artifacts so you will need to do 
some of the legwork yourself.

### 1.  Clone the repo


### 2.  Build the compiler plugin

```sbt analysisPlugin/assembly```

This will create a jar for the compiler plugin.

You will need to find the full path for the file e.g. 

```/workspace/ScalaClean/analysisPlugin/target/scala-2.12/analysisPlugin_2.12.8-0.1.0-SNAPSHOT-assembly.jar```

### 3. Update your build to pull in semanticDB and the ScalaClean plugin.

add ```addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.6")``` to your ```project\plugins.sbt```
(n.b. This should probably be just semanticDB plugin)

Manually add the ScalaClean plugin to your project definition(s) - this will be automated later:

```  .settings(
       scalacOptions  ++= {
         val srcLocations = (sourceDirectories in Compile).value.mkString(java.io.File.pathSeparator)
         Seq(
           "-Xplugin:/workspace/ScalaClean/analysisPlugin/target/scala-2.12/analysisPlugin_2.12.8-0.1.0-SNAPSHOT-assembly.jar",
           s"-P:scalaclean-analysis-plugin:srcdirs:${srcLocations}",
         )
       }
```

### 4. Now build your project 

All being well you should now have a number of files in your META-INF directory:

E.g. for akka-actor in ```/akka-actor/target/classes/META-INF/``` I have:

```
./akka-actor/target/classes/META-INF//semanticdb
./akka-actor/target/classes/META-INF//semanticdb/akka-actor/...lots of semanticdb files
./akka-actor/target/classes/META-INF//ScalaClean
./akka-actor/target/classes/META-INF//ScalaClean/scalaclean-relationships.csv
./akka-actor/target/classes/META-INF//ScalaClean/ScalaClean.properties
./akka-actor/target/classes/META-INF//ScalaClean/scalaclean-elements.csv
```

### 5. Build the ScalaClean main:

In ScalaClean ```sbt command/assembly```

Creates: ```/workspace/ScalaClean/command/target/scala-2.12/command-assembly-0.1.0-SNAPSHOT.jar``` (your location may vary)

Now you can run ScalaClean using this jar.  Point ScalaClean at your projects ```ScalaClean.properties``` files e.g.:

```java -jar $SCALACLEAN_JAR deadcode `find myproject -name "ScalaClean.properties"```

By default it will print out the diff that it computes.  To apply the changes directly add ```--replace``` to the command
before the list of files.  *BEWARE* - This is destructive - do not run it on code you care about that you
do not already.

### 6. Report the bugs

Look at the output, determine if the output does not meet your expectations (one of the steps failed or the result is broken wrong) then 
drop us a bug report - we would love to hear about it.  If you are feeling especially kind
a bit of investigation and a sample project (see ```testProjects/deadCodeXXX``` for examples that we are using for testing. ) 

 
## Useful links

- [ScalaFix](https://github.com/scalacenter/scalafix)
- [ScalaMeta/SemanticDB](https://scalameta.org/)

## Setup instructions

Open in Intellij - not all tests are passing at this point


