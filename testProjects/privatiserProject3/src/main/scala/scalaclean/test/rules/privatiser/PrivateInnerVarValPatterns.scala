package scalaclean.test.rules.privatiser

object PrivateInnerVarValPatterns extends App {

  //not used = should be marked private[this
  val (a1, b1, c1) = (1, 2, 3)

  //private - only used in within PrivateInnerVarValPatterns
  val (a2, b2, c2) = (1, 2, 3)

  //private - b3 accessed only used in within PrivateInnerVarValPatterns
  val (a3, b3, c3) = (1, 2, 3)

  //private - only used in within PrivateInnerVarValPatterns
  var (a12, b12, c12) = (1, 2, 3)

  //private - b13 accessed only used in within PrivateInnerVarValPatterns
  var (a13, b13, c13) = (1, 2, 3)

  object usage {
    print(a2 + b2 + c2)
    print(b3)
    print(a12 + b12 + c12)
    print(b13)
    print(Inner.b2)
  }

  object Inner {
    // used in Inner
    val (a1,b1,c1) = (1,2,3)
    println(this.b1)

    // used in PrivateInnerVarValPatterns.usage
    val (a2,b2,c2) = (1,2,3)
  }
}
