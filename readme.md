# ScalaClean

ScalaClean is a set of rules for ScalaFix that do deeper code of a codebase 
based on ScalaFix and SemanticDB.

## Current Status

In development - not yet working. 

## Ideas

- Dead code detection removal
- adjust method/class/package visability to minimum
- find and remove unused parameters (e.g. overriding method but parameter is not used anywhere)
- Many more

## TODO List

- [ ] Create extension methods in ScalaFix to support pre-analysis
- [ ] Generate graph structure (packages/classes/methods/references)
- [ ] Work out how to write automated tests (maybe debug text in result?
- [ ] Apply detected code rules


## Useful links

- [ScalaFix](https://github.com/scalacenter/scalafix)
- [ScalaMeta/SemanticDB](https://scalameta.org/)

## Setup instructions

We rely on a tweaked version of ScalaFix at the moment, so you need to clone it
and publish it locally:

1. Fork and clone: https://github.com/rorygraves/scalafix
    1. This will create a package scalafix
         
1. (Within scalafix) Checkout branch ```rule_begin_end```
1. (Within scalafix) )Run ```sbt publishLocal``` - If this fails check that JAVA_HOME is set to be a JDK root
1. Make a note of the version number published e.g. ```0.9.1+5-6bbf4f13-SNAPSHOT```
2. In workspace fork and clone ```https://github.com/rorygraves/ScalaClean`````
2. (in ScalaClean)  - ensure version in project/plugins.sbt matches version above
3. Open ScalaClean in Intellij)
4. Run RuleSuite



