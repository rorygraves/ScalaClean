trait GrandParentTrait {  def d1: Int}
trait ParentTrait extends GrandParentTrait {  def d1: Int}
object Object_ChildTrait extends ParentTrait {override def d1: Int = ???}
object x