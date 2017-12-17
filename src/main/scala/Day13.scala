package aoc

object Day13 {
  val fileURL = getClass.getResource(".") + "/Day13input.txt"
  val in = Util.readInputURL(fileURL)

  def parseInput(lines: List[String]): Map[Int, Int] = {
    lines.map(l => {
      val tokens = l.split("[ :]")
      (tokens(0).toInt, tokens(2).toInt)
    }).toMap
  }

  object Direction extends Enumeration {
    val UP = 0
    val DOWN = 1
  }

  case class SecurityScanner(range: Int, pos: Int, direction: Int) {
    def tick: SecurityScanner = {
      if(direction == Direction.DOWN)
        if(pos + 1 > range) SecurityScanner(range, pos - 1, Direction.UP)
        else SecurityScanner(range, pos + 1, Direction.DOWN)
      else
        if(pos - 1 == 0) SecurityScanner(range, pos + 1, Direction.DOWN)
        else SecurityScanner(range, pos - 1, Direction.UP)
    }

    def isTop: Boolean = pos == 1
  }

  /** A layer in the firewall.
    *
    * @param depth the depth of that layer in the firewall (0 .. maxDepth)
    * @param securityScanner the current securityScanner
    */
  case class Layer(depth: Int, securityScanner: SecurityScanner) {
    def tick: Layer = Layer(depth, securityScanner.tick)
  }

  /** Firewall companion object.
    */
  object FireWall {
    /** Build a firewall.
      *
      * @note If the layer has no range (is not defined in the input), a [[Layer]] with range 0 is created.
      * @note For all layers the security scanner starts at position 1 (the top).
      *
      * @param inputLayers the layers to use to build the firewall
      * @return the firewall
      */
    def build(inputLayers: Map[Int, Int]): FireWall = {
      val layers = (for {
        l <- 0 to inputLayers.keys.max
        r = inputLayers.getOrElse(l, 0)
      } yield Layer(l, SecurityScanner(r, 1, Direction.DOWN))).toList
      FireWall(layers, 0, 0)
    }
  }

  /** The firewall.
    *
    * A firewall is build from layers.
    *
    * @param layers
    * @param threadPosition
    * @param securityScore
    */
  case class FireWall(layers: List[Layer], threadPosition: Int, securityScore: Int) {
    def tick: FireWall = {
      val newSecurityScore = layers.map(l => {
        //println(s"${l.depth}/${l.securityScanner.range}/${threadPosition}/${l.securityScanner.pos}")
        if(threadPosition == l.depth && l.securityScanner.isTop) l.depth * l.securityScanner.range
        else 0
      }).sum
      FireWall(layers.map(_.tick), threadPosition + 1, securityScore + newSecurityScore)
    }

    override def toString: String = {
      layers.map(l => {
        val depthStr = if(l.depth == threadPosition) s"(${l.depth}): " else s" ${l.depth} : "
        val rangeStr = (for(r <- 1 to l.securityScanner.range) yield {
          if(l.securityScanner.pos == r) "[S]" else "[ ]"
        }).mkString
        depthStr + rangeStr
      }).mkString("\n")
    }
  }
}
