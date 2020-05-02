object DynamicReprTuplesCodeGen {

  def generate: List[String] = {
    val capitals                = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    def decN(gen: Char): String = s"dec$gen: Decode[$gen]"

    val header = """// $COVERAGE-OFF$should find a way to test all of them ..."""
    val footer = """// $COVERAGE-ON$"""
    val lines = (2 to 22).map { n =>
      val tupleTypeArgs = (1 to n).map(_ => "DynamicRepr").mkString(", ")
      val genericArgs   = (1 to n).map(i => capitals(i - 1)).mkString(", ")
      val decoders      = (1 to n).map(i => decN(capitals(i - 1))).mkString(", ")
      val arguments     = (1 to n).map(i => s"t$i").mkString(",")
      val convertN      = (1 to n).map(i => s"t$i.convert[${capitals(i - 1)}]").mkString(",")

      s"""
         |implicit class DynamicReprTuples$n(tuple: Tuple$n[$tupleTypeArgs]) {
         |    def applyT[$genericArgs, RR](f: ($genericArgs) => Try[RR])(
         |      implicit $decoders
         |    ): Result[RR] = {
         |      val ($arguments) = tuple
         |      product$n($convertN).map(f.tupled).flatMap(_.toEither.left.map(t=>WrappedError(t)))
         |    }
         |
         |    def applyO[$genericArgs, RR](f: ($genericArgs) => Option[RR])(
         |      implicit $decoders
         |    ): Result[RR] = {
         |      val ($arguments) = tuple
         |      product$n($convertN).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
         |    }
         |
         |    def applyE[$genericArgs, RL, RR](f: ($genericArgs) => Either[RL, RR])(
         |      implicit $decoders
         |    ): Result[RR] = {
         |      val ($arguments) = tuple
         |      product$n($convertN).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
         |    }
         |
         |    def apply[$genericArgs, RR](f: ($genericArgs) => RR)(
         |      implicit $decoders
         |    ): Result[RR] = {
         |      val ($arguments) = tuple
         |      product$n($convertN).map(f.tupled)
         |    }
         |  }
         |  """.stripMargin
    }.toList

    header :: lines ::: List(footer)
  }

}
