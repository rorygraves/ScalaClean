package overrideval

import java.io.ObjectInputStream

object OverrideVals extends App {

  val usage = Seq(Container.ByteString1C).map(x => x.SerializationIdentity  + x.SerializationIdentity2)
  println(usage)
}

object Container {

  private[overrideval] sealed trait Companion {
    def SerializationIdentity: Byte
    def SerializationIdentity2: Byte
  }

  private[overrideval] object ByteString1C extends Companion {
    val SerializationIdentity = 1.toByte
    override val SerializationIdentity2 = 2.toByte
  }

}
