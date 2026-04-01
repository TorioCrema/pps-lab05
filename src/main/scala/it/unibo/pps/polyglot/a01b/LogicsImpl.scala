package it.unibo.pps.polyglot.a01b

import it.unibo.pps.polyglot.OptionToOptional
import it.unibo.pps.polyglot.a01b.Logics
import it.unibo.pps.util.Optionals.{Optional, Optional as ScalaOptional}
import it.unibo.pps.util.Sequences.Sequence
import Sequence.*

import scala.util.Random
import scala.jdk.javaapi.OptionConverters

trait Logics:
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won(): Boolean

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  opaque type Coords = (x: Int, y: Int)
  private val mineMap: Sequence[Coords] = createMap()
  private var hitCount = 0

  def hit(x: Int, y: Int): java.util.Optional[Integer] = {
    this.hitCount += 1
    if mineMap.contains((x, y)) then
      OptionToOptional(Optional.Empty())
    else
      OptionToOptional(Optional.Just(mineMap.filter(bomb => math.abs(bomb.x - x) <= 1 && math.abs(bomb.y - y) <= 1).len()))
  }

  def won(): Boolean = this.mines + this.hitCount == this.size * this.size

  private def createMap(): Sequence[Coords] =
    var map: Sequence[Coords] = Nil()
    for i <- 1 to mines do map = map.concat(Cons(getFreePosition(map), Nil()))
    map

  private def getFreePosition(map: Sequence[Coords]): Coords =
    var x: Int = Random.nextInt(size - 1)
    var y: Int = Random.nextInt(size - 1)
    while (mineMap.contains((x, y)))
      x = Random.nextInt(size - 1)
      y = Random.nextInt(size - 1)
    (x, y)