/*
rules = [ Analysis , Privatiser ]

*/
package x1.scalaclean.test.rules.privatiser.MakePublic {

  object AppPublic extends App {
    import x2.scalaclean.test.rules.privatiser.MakePublic.Other._
    println(myVal + myVar + myDef + myObj.toString + (new myClass + (new myTrait{}).toString))
    import x2.scalaclean.test.rules.privatiser.MakePublic.Other3.{myVal => myValOn3}
    println(myValOn3)
    x2.scalaclean.test.rules.privatiser.MakePublic.Access.access2()
    x2.scalaclean.test.rules.privatiser.MakePublic.Access.access3()
  }

}
package x2.scalaclean.test.rules.privatiser.MakePublic {

  object Other {
    val myVal = 1
    var myVar = 1
    def myDef = 1
    object myObj
    trait myTrait
    class myClass
  }
  object Other2 {
    val myVal = 1
    var myVar = 1
    def myDef = 1
    object myObj
    trait myTrait
    class myClass
  }
  object Other3 {
    val myVal = 1
    var myVar = 1
    def myDef = 1
    object myObj
    trait myTrait
    class myClass
  }

  import x2.scalaclean.test.rules.privatiser.MakePublic.Other2._
  object Access{
    def access2() = {
      import x2.scalaclean.test.rules.privatiser.MakePublic.Other2._
      println(myVal + myVar + myDef + myObj.toString + (new myClass + (new myTrait {}).toString))
    }
    def access3() = {
      import x2.scalaclean.test.rules.privatiser.MakePublic.Other2._
      println(myVal + myVar + myDef + myObj.toString + (new myClass + (new myTrait {}).toString))
    }
  }


}
