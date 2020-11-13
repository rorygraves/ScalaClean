package scalaclean.model.impl

import java.util.concurrent.ConcurrentHashMap

import scalaclean.model.ElementId

/**
  * Some expensive to calculate, but infrequently used values are stored in the cache rather then on each element
  */
private[impl] object Caches {
  private val xtendsCache = new ConcurrentHashMap[(ElementId, ClassLikeImpl), Boolean]
  def xtends(symbol: ElementId, cls: ClassLikeImpl): Boolean =
    xtendsCache.computeIfAbsent((symbol, cls), x => x._2.extendsElementId(filter = Some(_.elementId == x._1)).hasNext)

  //TODO should probably be a Member
  private val overridesExternalCache = new ConcurrentHashMap[ElementModelImpl, Boolean]
  def overridesExternal(ele: ElementModelImpl): Boolean =
    overridesExternalCache.computeIfAbsent(ele, m => m.overridesFull().exists( _.elementIfDefined.fold(true)(_.overridesExternal)))

}
