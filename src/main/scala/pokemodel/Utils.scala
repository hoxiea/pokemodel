package pokemodel

import scala.util.Random

object Utils {
  def intBetween(lower : Int, upper : Int) = {
    // Generate an Int between lower (inclusive) and upper (exclusive)
    require (lower <= upper, "invalid args to intBetween")
    lower + Random.nextInt(upper - lower)
  }
}