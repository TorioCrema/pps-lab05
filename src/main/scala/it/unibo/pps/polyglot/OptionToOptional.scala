package it.unibo.pps.polyglot

import it.unibo.pps.util.Optionals.Optional as ScalaOptional

import java.util.Optional
object OptionToOptional:
  def apply(option: ScalaOptional[Int]): Optional[Integer] = option match
    case ScalaOptional.Just(a) => Optional.of(a)
    case _ => Optional.empty()
