# ScalaClean

ScalaClean is a full program static analysis tool.
By looking at the entire program as a single entity we can do an in-depth analysis that is not 
available when working file by file as most analysis toos do.

## Current Status

Runnable, but probably will do something bad ;).

## Ideas

- Dead code detection removal
- adjust method/class/package visibility to minimum
- find and remove unused parameters (e.g. overriding method but parameter is not used anywhere)
- Many more

## TODO List

- [ ] Run it on more things an fix rhe bugs


## Useful links

- [ScalaFix](https://github.com/scalacenter/scalafix)
- [ScalaMeta/SemanticDB](https://scalameta.org/)

## Setup instructions

Open in Intellij - not all tests are passing at this point


## Overall design
We structure each feature based on a common Analysis and a custom rule: 

#### Analysis
Using Scalafix, SemanticDB and reflection we generate a graph that represents the syntatic structure of the the code, including cross-references, so  model of our code and how it operates internally and with the libraries that it uses. 
  
#### Custom Rule
We can develop a custom rule, to focus on a specific aspect, or opportunity for improvement
The custom rule can use the analysis model to identify improvements, generally by colouring of the syntax tree withe the results of the analysis
The rule then traverses the syntax graph and suggests improvements using the ScalaFix toolkit
such as access modifiers changes, and detection of possibly unused code.

After generating the suggestions, we can use ScalaFix to write rules that can rewrite and adjust our code. 



