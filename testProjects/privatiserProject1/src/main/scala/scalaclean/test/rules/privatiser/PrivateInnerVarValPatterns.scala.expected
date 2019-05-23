package scalaclean.test.rules.privatiser

object PrivateInnerVarValPatterns extends App {

  //not used = should be flagged with comments
  /* *** SCALA CLEAN can't detect usage */val (a1, b1, c1) = (1, 2, 3)

  //private - only used in within PrivateInnerVarValPatterns
  private val (a2, b2, c2) = (1, 2, 3)

  //private - b3 accessed only used in within PrivateInnerVarValPatterns
  private val (a3, b3, c3) = (1, 2, 3)

  //private - only used in within PrivateInnerVarValPatterns
  private var (a12, b12, c12) = (1, 2, 3)

  //private - b13 accessed only used in within PrivateInnerVarValPatterns
  private var (a13, b13, c13) = (1, 2, 3)

  /* *** SCALA CLEAN can't detect usage */object usage {
    print(a2 + b2 + c2)
    print(b3)
    print(a12 + b12 + c12)
    print(b13)
    print(Inner.b2)
  }

  private object Inner {
    // used in Inner
    private val (a1,b1,c1) = (1,2,3)
    println(this.b1)

    // used in PrivateInnerVarValPatterns.usage
    private[PrivateInnerVarValPatterns] val (a2,b2,c2) = (1,2,3)
  }
}
