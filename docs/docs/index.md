# Welcome to ScalaClean documentation

Right now this is pretty much developer notes.  Longer term it might be something more.

## Deploying the docs


Docs are created with  [mkdocs.org](https://mkdocs.org) from the ```docs``` directory.

To deploy updated docs run (from the ```docs``` directory)

```bash
 mkdocs gh-deploy
```


## Custom SemanticDB ScalaC Plugin

### Location

[Github scalameta scalaclean](https://github.com/rorygraves/scalameta/tree/scalaclean)

Clone this repo.

### Compiling custom semanticdb-scalac plugin

```
sbt -J-Dscalameta.version=9.9.9-SNAPSHOT
> scalameta/publishLocal
> semanticdbScalacCore/publishLocal
> semanticdbScalacPlugin/publishLocal
```

sbt -J-Dscalameta.version=9.9.9-SNAPSHOT scalameta/publishLocal  semanticdbScalacCore/publishLocal  semanticdbScalacPlugin/publishLocal



```
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "9.9.9-SNAPSHOT" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
```

The dead code build has been updated to use this version.

Key class: SemanticdbTyperComponent - toTextDocument
Bulk of the wrk in TextDocumentOps

Next steps - determine CSV formats and contents - output these directly.
