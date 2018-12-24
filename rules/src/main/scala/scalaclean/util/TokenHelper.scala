package scalaclean.util

import scala.meta.tokens.{Token, Tokens}

object TokenHelper {

  /** Walking back from a given Token, capture all whitespace tokens immediately before.
    *
    * @param targetToken The target token
    * @param tokens The entire document of tokens
    * @return A sequence of tokens representing the whitespace immediately preceding the target token.
    */
  def whitespaceTokensBefore(targetToken: Token, tokens: Tokens): List[Token] = {
    def isWhiteSpace(token: Token): Boolean = {
      token match {
        case _ : Token.Space => true
        case _ : Token.Tab => true
        case _ : Token.CR => true
        case _ : Token.LF => true
        case _ : Token.FF => true
        case _ => false
      }
    }

    // This is a bit ugly but pretty efficient as 'tokens' might be huge.
    val idx = tokens.indexOf(targetToken)
    if(idx == -1)
      List.empty
    else {
      var curIndex = idx-1
      while(curIndex >= 0 && isWhiteSpace(tokens(curIndex))) {
        curIndex -= 1
      }

      tokens.slice(curIndex+1,idx -1).toList
    }
  }
}
