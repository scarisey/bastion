/*
 * Copyright 2020 dev.scarisey
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package bastion

import bastion.ResultProducts._

import scala.util.Try

trait DecodingStateTuples {

  implicit class DecodingStateOps(state: DecodingState) {

    /**
     * For a function f, mapping the type A to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for type A enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, RR](f: A => Try[RR])(
      implicit decA: Decoder[A]
    ): Result[RR] =
      state.runDecoder[A].flatMap(r => f(r).toEither.left.map(t => WrappedError(t)))

    /**
     * For a function f, mapping the type A to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for type A enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, RR](f: A => Option[RR])(
      implicit decA: Decoder[A]
    ): Result[RR] =
      state.runDecoder[A].flatMap(r => f(r).toRight(NilSmartConstructorError))

    /**
     * For a function f, mapping the type A to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for type A, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, RL, RR](f: A => Either[RL, RR])(
      implicit decA: Decoder[A]
    ): Result[RR] =
      state.runDecoder[A].flatMap(r => f(r).left.map(WrappedError(_)))

    /**
     * For a function f, mapping type A to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for type A, enabling decoding of complex types.
     */
    def apply[A, RR](f: A => RR)(
      implicit decA: Decoder[A]
    ): Result[RR] =
      state.runDecoder[A].map(f)
  }

  // /start/producthelper/ - DO NOT REMOVE
// $COVERAGE-OFF$should find a way to test all of them ...

  implicit class DecodingStateTuples2(tuple: Tuple2[DecodingState, DecodingState]) {

    /**
     * For a function f, mapping the types A, B to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, RR](f: (A, B) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B]
    ): Result[RR] = {
      val (t1, t2) = tuple
      product2(t1.runDecoder[A], t2.runDecoder[B]).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, RR](f: (A, B) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B]
    ): Result[RR] = {
      val (t1, t2) = tuple
      product2(t1.runDecoder[A], t2.runDecoder[B]).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, RL, RR](f: (A, B) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B]
    ): Result[RR] = {
      val (t1, t2) = tuple
      product2(t1.runDecoder[A], t2.runDecoder[B]).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, enabling decoding of complex types.
     */
    def apply[A, B, RR](f: (A, B) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B]
    ): Result[RR] = {
      val (t1, t2) = tuple
      product2(t1.runDecoder[A], t2.runDecoder[B]).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples3(tuple: Tuple3[DecodingState, DecodingState, DecodingState]) {

    /**
     * For a function f, mapping the types A, B, C to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, RR](f: (A, B, C) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C]
    ): Result[RR] = {
      val (t1, t2, t3) = tuple
      product3(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C])
        .map(f.tupled)
        .flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, RR](f: (A, B, C) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C]
    ): Result[RR] = {
      val (t1, t2, t3) = tuple
      product3(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C]).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, RL, RR](f: (A, B, C) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C]
    ): Result[RR] = {
      val (t1, t2, t3) = tuple
      product3(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C]).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, enabling decoding of complex types.
     */
    def apply[A, B, C, RR](f: (A, B, C) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C]
    ): Result[RR] = {
      val (t1, t2, t3) = tuple
      product3(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C]).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples4(tuple: Tuple4[DecodingState, DecodingState, DecodingState, DecodingState]) {

    /**
     * For a function f, mapping the types A, B, C, D to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, RR](f: (A, B, C, D) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D]
    ): Result[RR] = {
      val (t1, t2, t3, t4) = tuple
      product4(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D])
        .map(f.tupled)
        .flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, RR](f: (A, B, C, D) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D]
    ): Result[RR] = {
      val (t1, t2, t3, t4) = tuple
      product4(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D])
        .map(f.tupled)
        .flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, RL, RR](f: (A, B, C, D) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D]
    ): Result[RR] = {
      val (t1, t2, t3, t4) = tuple
      product4(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D])
        .map(f.tupled)
        .flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, enabling decoding of complex types.
     */
    def apply[A, B, C, D, RR](f: (A, B, C, D) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D]
    ): Result[RR] = {
      val (t1, t2, t3, t4) = tuple
      product4(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D]).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples5(tuple: Tuple5[DecodingState, DecodingState, DecodingState, DecodingState, DecodingState]) {

    /**
     * For a function f, mapping the types A, B, C, D, E to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, RR](f: (A, B, C, D, E) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5) = tuple
      product5(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D], t5.runDecoder[E])
        .map(f.tupled)
        .flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, RR](f: (A, B, C, D, E) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5) = tuple
      product5(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D], t5.runDecoder[E])
        .map(f.tupled)
        .flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, RL, RR](f: (A, B, C, D, E) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5) = tuple
      product5(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D], t5.runDecoder[E])
        .map(f.tupled)
        .flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, RR](f: (A, B, C, D, E) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5) = tuple
      product5(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D], t5.runDecoder[E]).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples6(
    tuple: Tuple6[DecodingState, DecodingState, DecodingState, DecodingState, DecodingState, DecodingState]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, RR](f: (A, B, C, D, E, F) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6) = tuple
      product6(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D], t5.runDecoder[E], t6.runDecoder[F])
        .map(f.tupled)
        .flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, RR](f: (A, B, C, D, E, F) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6) = tuple
      product6(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D], t5.runDecoder[E], t6.runDecoder[F])
        .map(f.tupled)
        .flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, RL, RR](f: (A, B, C, D, E, F) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6) = tuple
      product6(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D], t5.runDecoder[E], t6.runDecoder[F])
        .map(f.tupled)
        .flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, RR](f: (A, B, C, D, E, F) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6) = tuple
      product6(t1.runDecoder[A], t2.runDecoder[B], t3.runDecoder[C], t4.runDecoder[D], t5.runDecoder[E], t6.runDecoder[F])
        .map(f.tupled)
    }
  }

  implicit class DecodingStateTuples7(
    tuple: Tuple7[DecodingState, DecodingState, DecodingState, DecodingState, DecodingState, DecodingState, DecodingState]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, RR](f: (A, B, C, D, E, F, G) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7) = tuple
      product7(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, RR](f: (A, B, C, D, E, F, G) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7) = tuple
      product7(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, RL, RR](f: (A, B, C, D, E, F, G) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7) = tuple
      product7(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, RR](f: (A, B, C, D, E, F, G) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7) = tuple
      product7(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples8(
    tuple: Tuple8[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, RR](f: (A, B, C, D, E, F, G, H) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8) = tuple
      product8(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, RR](f: (A, B, C, D, E, F, G, H) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8) = tuple
      product8(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, RL, RR](f: (A, B, C, D, E, F, G, H) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8) = tuple
      product8(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, RR](f: (A, B, C, D, E, F, G, H) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8) = tuple
      product8(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples9(
    tuple: Tuple9[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, RR](f: (A, B, C, D, E, F, G, H, I) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9) = tuple
      product9(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, RR](f: (A, B, C, D, E, F, G, H, I) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9) = tuple
      product9(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, RL, RR](f: (A, B, C, D, E, F, G, H, I) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9) = tuple
      product9(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, RR](f: (A, B, C, D, E, F, G, H, I) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9) = tuple
      product9(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples10(
    tuple: Tuple10[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, RR](f: (A, B, C, D, E, F, G, H, I, J) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = tuple
      product10(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, RR](f: (A, B, C, D, E, F, G, H, I, J) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = tuple
      product10(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, RL, RR](f: (A, B, C, D, E, F, G, H, I, J) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = tuple
      product10(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, RR](f: (A, B, C, D, E, F, G, H, I, J) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = tuple
      product10(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples11(
    tuple: Tuple11[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, RR](f: (A, B, C, D, E, F, G, H, I, J, K) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = tuple
      product11(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, RR](f: (A, B, C, D, E, F, G, H, I, J, K) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = tuple
      product11(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, RL, RR](f: (A, B, C, D, E, F, G, H, I, J, K) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = tuple
      product11(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, RR](f: (A, B, C, D, E, F, G, H, I, J, K) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = tuple
      product11(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples12(
    tuple: Tuple12[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = tuple
      product12(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = tuple
      product12(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, RL, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = tuple
      product12(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = tuple
      product12(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples13(
    tuple: Tuple13[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = tuple
      product13(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = tuple
      product13(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, RL, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = tuple
      product13(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = tuple
      product13(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples14(
    tuple: Tuple14[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = tuple
      product14(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = tuple
      product14(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RL, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Either[RL, RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = tuple
      product14(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = tuple
      product14(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples15(
    tuple: Tuple15[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Try[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = tuple
      product15(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Option[RR])(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = tuple
      product15(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Either[RL, RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = tuple
      product15(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = tuple
      product15(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples16(
    tuple: Tuple16[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Try[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = tuple
      product16(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Option[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = tuple
      product16(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Either[RL, RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = tuple
      product16(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => RR)(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = tuple
      product16(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples17(
    tuple: Tuple17[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Try[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = tuple
      product17(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Option[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = tuple
      product17(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Either[RL, RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = tuple
      product17(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => RR
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = tuple
      product17(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples18(
    tuple: Tuple18[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Try[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = tuple
      product18(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Option[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = tuple
      product18(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Either[RL, RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = tuple
      product18(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => RR
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = tuple
      product18(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples19(
    tuple: Tuple19[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Try[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = tuple
      product19(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Option[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = tuple
      product19(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Either[RL, RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = tuple
      product19(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => RR
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = tuple
      product19(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples20(
    tuple: Tuple20[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Try[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = tuple
      product20(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Option[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = tuple
      product20(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Either[RL, RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = tuple
      product20(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => RR
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = tuple
      product20(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples21(
    tuple: Tuple21[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Try[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T],
      decU: Decoder[U]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = tuple
      product21(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T],
        t21.runDecoder[U]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Option[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T],
      decU: Decoder[U]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = tuple
      product21(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T],
        t21.runDecoder[U]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Either[RL, RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T],
      decU: Decoder[U]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = tuple
      product21(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T],
        t21.runDecoder[U]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => RR
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T],
      decU: Decoder[U]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = tuple
      product21(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T],
        t21.runDecoder[U]
      ).map(f.tupled)
    }
  }

  implicit class DecodingStateTuples22(
    tuple: Tuple22[
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState,
      DecodingState
    ]
  ) {

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instances of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, enabling decoding of complex types.
     * The eventual throwable error will be wrapped in a [[WrappedError]].
     */
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Try[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T],
      decU: Decoder[U],
      decV: Decoder[V]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = tuple
      product22(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T],
        t21.runDecoder[U],
        t22.runDecoder[V]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V to maybe RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, enabling decoding of complex types.
     * The absence of value RR will be represented by a [[NilSmartConstructorError]].
     */
    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Option[RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T],
      decU: Decoder[U],
      decV: Decoder[V]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = tuple
      product22(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T],
        t21.runDecoder[U],
        t22.runDecoder[V]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    /**
     * For a function f, mapping the types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V to RR, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, enabling decoding of complex types.
     * The eventual error RL will be wrapped in a [[WrappedError]].
     */
    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Either[RL, RR]
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T],
      decU: Decoder[U],
      decV: Decoder[V]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = tuple
      product22(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T],
        t21.runDecoder[U],
        t22.runDecoder[V]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    /**
     * For a function f, mapping types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V to RR, create an instance of Decoder that will map a [[DynamicRepr]] to RR.
     * This method can use your own instance of Decoder for types A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, enabling decoding of complex types.
     */
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => RR
    )(
      implicit decA: Decoder[A],
      decB: Decoder[B],
      decC: Decoder[C],
      decD: Decoder[D],
      decE: Decoder[E],
      decF: Decoder[F],
      decG: Decoder[G],
      decH: Decoder[H],
      decI: Decoder[I],
      decJ: Decoder[J],
      decK: Decoder[K],
      decL: Decoder[L],
      decM: Decoder[M],
      decN: Decoder[N],
      decO: Decoder[O],
      decP: Decoder[P],
      decQ: Decoder[Q],
      decR: Decoder[R],
      decS: Decoder[S],
      decT: Decoder[T],
      decU: Decoder[U],
      decV: Decoder[V]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = tuple
      product22(
        t1.runDecoder[A],
        t2.runDecoder[B],
        t3.runDecoder[C],
        t4.runDecoder[D],
        t5.runDecoder[E],
        t6.runDecoder[F],
        t7.runDecoder[G],
        t8.runDecoder[H],
        t9.runDecoder[I],
        t10.runDecoder[J],
        t11.runDecoder[K],
        t12.runDecoder[L],
        t13.runDecoder[M],
        t14.runDecoder[N],
        t15.runDecoder[O],
        t16.runDecoder[P],
        t17.runDecoder[Q],
        t18.runDecoder[R],
        t19.runDecoder[S],
        t20.runDecoder[T],
        t21.runDecoder[U],
        t22.runDecoder[V]
      ).map(f.tupled)
    }
  }

// $COVERAGE-ON$
  //  /end/producthelper/ - DO NOT REMOVE
}
