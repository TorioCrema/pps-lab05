package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics
import it.unibo.pps.util.Sequences.Sequence, Sequence.*
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private var map: Sequence[(x: Int, y: Int)] = Nil()
  private val initial: (x: Int, y: Int) = (Random.nextInt(this.size - 1), Random.nextInt(this.size - 1))
  private var step = 0
  private val axes: Sequence[(x: Int, y: Int)] = Cons((0, 1), Cons((1, 1), Cons((1, 0), Cons((1, -1), Cons((0, -1), Cons((-1, -1), Cons((-1, 0), Cons((-1, 1), Nil()))))))))

  override def tick(): Unit =
    this.map = for
      axis <- this.axes
      length <- getSequence(this.step)
    yield (this.initial.x + axis.x * length, this.initial.y + axis.y * length)
    this.step += 1

  private def getSequence(downFrom: Int): Sequence[Int] = downFrom match
    case 0 => Cons(0, Nil())
    case n => Cons(n, getSequence(n - 1))

  override def isOver: Boolean = this.map.filter((x, y) => x < 0 || y < 0 || x >= this.size || y >= this.size) != Nil()

  override def hasElement(x: Int, y: Int): Boolean = map.contains((x, y))
