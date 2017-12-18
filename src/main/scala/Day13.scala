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

  case class SecurityScanner(range: Int, pos: Int = 1, direction: Int = Direction.DOWN) {
    def tick: SecurityScanner = {
      if (range > 1)
        if (direction == Direction.DOWN)
          if (pos + 1 > range) SecurityScanner(range, pos - 1, Direction.UP)
          else SecurityScanner(range, pos + 1, Direction.DOWN)
        else
          if (pos - 1 == 0) SecurityScanner(range, pos + 1, Direction.DOWN)
          else SecurityScanner(range, pos - 1, Direction.UP)
      else
        this
    }

    def isTop: Boolean = (pos == 1 && range > 0)
  }

  /** A layer in the firewall.
    *
    * @param depth           the depth of that layer in the firewall (0 .. maxDepth)
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
    def build(inputLayers: Map[Int, Int], delay: Int): FireWall = {
      val layers = (for {
        l <- 0 to inputLayers.keys.max
        r = inputLayers.getOrElse(l, 0)
      } yield Layer(l, SecurityScanner(r))).toList
      FireWall(layers, delay * (-1), 0, false)
    }

    def runSimulation(fw: FireWall): FireWall = {
      def go(fw: FireWall, count: Int): FireWall = {
        //println(count); println(fw); println("---")
        if (count <= 0) fw.tick
        else go(fw.tick, count - 1)
      }

      go(fw, Math.abs(fw.threatPosition) + fw.layers.size)
    }
  }

  /** The firewall.
    *
    * A firewall is build from layers.
    *
    * @param layers
    * @param threatPosition
    * @param securityScore
    */
  case class FireWall(layers: List[Layer], threatPosition: Int, securityScore: Int, threatDetected: Boolean) {
    def tick: FireWall = {
      val newThreats = layers.map(l => {
        //println(s"${l.depth}/${l.securityScanner.range}/${threadPosition}/${l.securityScanner.pos}")
        if (threatPosition == l.depth && l.securityScanner.isTop) (l.depth * l.securityScanner.range, true)
        else (0, false)
      })
      val newSecurityScore = newThreats.map(_._1).sum
      val newThreatDetected = !(newThreats.map(_._2).forall(!_))
      FireWall(layers.map(_.tick), threatPosition + 1, securityScore + newSecurityScore, threatDetected || newThreatDetected)
    }

    override def toString: String = {
      layers.map(l => {
        val depthStr = if (l.depth == threatPosition) s"(${l.depth}): " else s" ${l.depth} : "
        val rangeStr = (for (r <- 1 to l.securityScanner.range) yield
          if (l.securityScanner.pos == r) "[S]" else "[ ]"
          ).mkString
        depthStr + rangeStr
      }).mkString("\n")
    }
  }

  def findWayThrough(input: Map[Int, Int]): Int = {
    def go(input: Map[Int, Int], delay: Int): Int = {
      val fw = FireWall.runSimulation(FireWall.build(input, delay))
      if (fw.threatDetected) go(input, delay + 1)
      else delay
    }

    go(input, 0)
  }
}
