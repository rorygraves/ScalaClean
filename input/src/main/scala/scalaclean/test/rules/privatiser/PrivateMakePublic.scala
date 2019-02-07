/*
rules = [ Analysis , Privatiser ]

*/
package x1.scalaclean.test.rules.privatiser.MakePublic {

  object AppPublic extends App {
    import x2.scalaclean.test.rules.privatiser.MakePublic.Other._
    println(myVal + myVar + myDef+ (new myClass + (new myTrait{}).toString))
  }

}
package x2.scalaclean.test.rules.privatiser.MakePublic {

  object Other {
    val myVal= 1
    var myVar = 1
    def myDef = 1
    object myObj
    trait myTrait
    class myClass
  }

}
