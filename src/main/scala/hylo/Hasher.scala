package hylo

import scala.util.Random

/** A universal hash function. */
final class Hasher private {

  /** The currently computed hash value. */
  private var hash = Hasher.offsetBasis

  /** Returns the computed hash value. */
  def finalizeHash(): Int =
    hash

  /** Adds `n` to the computed hash value. */
  def combine(n: Int): Unit =
    hash = hash ^ n
    hash = hash * Hasher.prime

}

object Hasher {

  private val offsetBasis = 0x811c9dc5
  private val prime = 0x01000193

  /** A random seed ensuring different hashes across multiple runs. */
  private lazy val seed = scala.util.Random.nextInt()

  /** Creates an instance with the given `seed`. */
  def apply(): Hasher =
    val h = new Hasher()
    h.combine(seed)
    h

}
