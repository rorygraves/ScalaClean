# ScalaClean

ScalaClean is a set of rules for ScalaFix that do more in-depth code analysis of a codebase 
based on ScalaFix and SemanticDB.

## Current Status

In development - not yet working.

## Ideas

- Dead code detection removal
- adjust method/class/package visibility to minimum
- find and remove unused parameters (e.g. overriding method but parameter is not used anywhere)
- Many more

## TODO List

- [ ] Create extension methods in ScalaFix to support pre-analysis
- [ ] Generate graph structure (packages/classes/methods/references)
- [ ] Work out how to write automated tests (maybe debug text in result?)
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
1. (Within scalafix) )Run ```sbt publishLocal``` - If this fails, check that JAVA_HOME is set to be a JDK root
1. Make a note of the version number published, e.g. ```0.9.1+5-6bbf4f13-SNAPSHOT```
2. In workspace fork and clone ```https://github.com/rorygraves/ScalaClean`````
2. (in ScalaClean)  - ensure version in project/plugins.sbt matches version above
3. Open ScalaClean in Intellij)
4. Run RuleSuite

## Overall design
We structure each feature in two parts: 

#### Analysis
Using SemanticDB, we generate a graph that represents the key elements of our code and how they interact. 
This graph is then visited to perform an in-deep analysis of our code and suggest possible improvements 
such as access modifiers changes, and detection of possibly unused code.
  
#### Apply
After generating the suggestions, we can use ScalaFix to write rules that can rewrite and adjust our code. 



