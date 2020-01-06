package scalaclean.model


abstract sealed class NewElementId {
  def innerScopeString: String
  val id: String
  def debugValue: String = id

}
package impl {

  object NewElementIdImpl {

    import java.util.concurrent.ConcurrentHashMap

    val interned = new ConcurrentHashMap[String, NewElementIdImpl]

    val THIS = NewElementIdImpl("this@")
    val SOME_THIS = Some(THIS)

    def apply(id: String) = interned.computeIfAbsent(id, id => new NewElementIdImpl(id.intern()))
  }

  final class NewElementIdImpl private(val id: String) extends NewElementId {
    override def hashCode(): Int = id.hashCode()

    override def toString: String = s"ModelSymbol[$id]"

    override def innerScopeString: String = {
      val start = id.lastIndexOf('/')
      id.substring(start+1, id.indexOf('@', start))
    }
    val option = Some(this)
  }

}
