object ResultProductsCodeGen {
  def generate: List[String] = {
    val capitals = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    (3 to 22).map { n =>
      val genericArgs  = (1 to n).map(i => capitals(i - 1)).mkString(", ")
      val functionArgs = (0 until n).map(i => s"r${i + 1}: Result[${capitals(i)}]").mkString(", ")
      val productArguments = (1 until n).map(i => s"r$i").mkString(",")
      val argumentsChain   = (1 until n).map(i => s"x$i").mkString(",")
      val argumentsFlatten = (1 to n).map(i => s"x$i").mkString(",")

      s"""
         |  def product$n[$genericArgs]($functionArgs): Result[($genericArgs)] =
         |    product2(product${n-1}($productArguments),r$n) map {
         |       case (($argumentsChain),x$n) => ($argumentsFlatten)
         |    }""".stripMargin
    }.toList
  }
}
