package org.scalaclean.analysis

object IoTokens {
  val fileElements      = "scalaclean-elements.csv"
  val fileRelationships = "scalaclean-relationships.csv"

  val relExtends   = "ext"
  val relWithin    = "within"
  val relRefers    = "refers"
  val relOverrides = "overrides"
  val relGetter    = "getterFor"
  val relSetter    = "setterFor"

  val typeObject       = "object"
  val typeClass        = "class"
  val typeTrait        = "trait"
  val typeGetterMethod = "getter"
  val typeSetterMethod = "setter"
  val typePlainMethod  = "def"
  val typeVal          = "val"
  val typeVar          = "var"
  val typeFields       = "(fields)"
  val typeSource       = "source"

}
