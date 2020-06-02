object DynamicReprTuplesCodeGen {

  def generate: List[String] = {
    val capitals                = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    def decN(gen: Char): String = s"dec$gen: Decoder[$gen]"

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
         |    /**
         |     * For a function f, mapping the types $genericArgs to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
         |     * This method can use your own instances of Decoder for types $genericArgs, enabling decoding of complex types.
         |     * The eventual throwable error will be wrapped in a [[WrappedError]].
         |     */
         |    def applyT[$genericArgs, RR](f: ($genericArgs) => Try[RR])(
         |      implicit $decoders
         |    ): Result[RR] = {
         |      val ($arguments) = tuple
         |      product$n($convertN).map(f.tupled).flatMap(_.toEither.left.map(t=>WrappedError(t)))
         |    }
         |
         |    /**
         |     * For a function f, mapping the types $genericArgs to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
         |     * This method can use your own instance of Decoder for types $genericArgs, enabling decoding of complex types.
         |     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
         |     */
         |    def applyO[$genericArgs, RR](f: ($genericArgs) => Option[RR])(
         |      implicit $decoders
         |    ): Result[RR] = {
         |      val ($arguments) = tuple
         |      product$n($convertN).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
         |    }
         |
         |    /**
         |     * For a function f, mapping the types $genericArgs to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
         |     * This method can use your own instance of Decoder for types $genericArgs, enabling decoding of complex types.
         |     * The eventual error RL will be wrapped in a [[WrappedError]].
         |     */
         |    def applyE[$genericArgs, RL, RR](f: ($genericArgs) => Either[RL, RR])(
         |      implicit $decoders
         |    ): Result[RR] = {
         |      val ($arguments) = tuple
         |      product$n($convertN).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
         |    }
         |
         |    /**
         |     * For a function f, mapping types $genericArgs to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
         |     * This method can use your own instance of Decoder for types $genericArgs, enabling decoding of complex types.
         |     */
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
