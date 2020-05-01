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

package dev.scarisey.bastion

import dev.scarisey.bastion.ResultProducts._

import scala.util.Try

trait DynamicReprTuples {
  implicit class DynamicReprTuples1(t1: DynamicRepr) {
    def applyT[A, RR](f: A => Try[RR])(
      implicit decA: Decode[A]
    ): Result[RR] =
      for {
        tr1 <- t1.convert[A]
        r   <- f(tr1).toEither.left.map(t => WrappedError(t))
      } yield r

    def applyO[A, RR](f: A => Option[RR])(
      implicit decA: Decode[A]
    ): Result[RR] =
      for {
        tr1 <- t1.convert[A]
        r   <- f(tr1).toRight(NilSmartConstructorError)
      } yield r

    def applyE[A, RL, RR](f: A => Either[RL, RR])(
      implicit decA: Decode[A]
    ): Result[RR] =
      (for {
        tr1 <- t1.convert[A]
        r   <- f(tr1)
      } yield r).left.map(WrappedError(_))

    def apply[A, RR](f: A => RR)(
      implicit decA: Decode[A]
    ): Result[RR] =
      for {
        tr1 <- t1.convert[A]
      } yield f(tr1)
  }

  // /start/producthelper/ - DO NOT REMOVE
// $$COVERAGE-OFF$$should find a way to test all of them ...

  implicit class DynamicReprTuples2(tuple: Tuple2[DynamicRepr, DynamicRepr]) {
    def applyT[A, B, RR](f: (A, B) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B]
    ): Result[RR] = {
      val (t1, t2) = tuple
      product2(t1.convert[A], t2.convert[B]).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, RR](f: (A, B) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B]
    ): Result[RR] = {
      val (t1, t2) = tuple
      product2(t1.convert[A], t2.convert[B]).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, RL, RR](f: (A, B) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B]
    ): Result[RR] = {
      val (t1, t2) = tuple
      product2(t1.convert[A], t2.convert[B]).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, RR](f: (A, B) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B]
    ): Result[RR] = {
      val (t1, t2) = tuple
      product2(t1.convert[A], t2.convert[B]).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples3(tuple: Tuple3[DynamicRepr, DynamicRepr, DynamicRepr]) {
    def applyT[A, B, C, RR](f: (A, B, C) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C]
    ): Result[RR] = {
      val (t1, t2, t3) = tuple
      product3(t1.convert[A], t2.convert[B], t3.convert[C]).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, RR](f: (A, B, C) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C]
    ): Result[RR] = {
      val (t1, t2, t3) = tuple
      product3(t1.convert[A], t2.convert[B], t3.convert[C]).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, RL, RR](f: (A, B, C) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C]
    ): Result[RR] = {
      val (t1, t2, t3) = tuple
      product3(t1.convert[A], t2.convert[B], t3.convert[C]).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, RR](f: (A, B, C) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C]
    ): Result[RR] = {
      val (t1, t2, t3) = tuple
      product3(t1.convert[A], t2.convert[B], t3.convert[C]).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples4(tuple: Tuple4[DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr]) {
    def applyT[A, B, C, D, RR](f: (A, B, C, D) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D]
    ): Result[RR] = {
      val (t1, t2, t3, t4) = tuple
      product4(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D])
        .map(f.tupled)
        .flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, RR](f: (A, B, C, D) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D]
    ): Result[RR] = {
      val (t1, t2, t3, t4) = tuple
      product4(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D])
        .map(f.tupled)
        .flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, RL, RR](f: (A, B, C, D) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D]
    ): Result[RR] = {
      val (t1, t2, t3, t4) = tuple
      product4(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D]).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, RR](f: (A, B, C, D) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D]
    ): Result[RR] = {
      val (t1, t2, t3, t4) = tuple
      product4(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D]).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples5(tuple: Tuple5[DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr]) {
    def applyT[A, B, C, D, E, RR](f: (A, B, C, D, E) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5) = tuple
      product5(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E])
        .map(f.tupled)
        .flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, RR](f: (A, B, C, D, E) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5) = tuple
      product5(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E])
        .map(f.tupled)
        .flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, RL, RR](f: (A, B, C, D, E) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5) = tuple
      product5(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E])
        .map(f.tupled)
        .flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, RR](f: (A, B, C, D, E) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5) = tuple
      product5(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E]).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples6(tuple: Tuple6[DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr]) {
    def applyT[A, B, C, D, E, F, RR](f: (A, B, C, D, E, F) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6) = tuple
      product6(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E], t6.convert[F])
        .map(f.tupled)
        .flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, RR](f: (A, B, C, D, E, F) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6) = tuple
      product6(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E], t6.convert[F])
        .map(f.tupled)
        .flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, RL, RR](f: (A, B, C, D, E, F) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6) = tuple
      product6(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E], t6.convert[F])
        .map(f.tupled)
        .flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, RR](f: (A, B, C, D, E, F) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6) = tuple
      product6(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E], t6.convert[F]).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples7(
    tuple: Tuple7[DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr]
  ) {
    def applyT[A, B, C, D, E, F, G, RR](f: (A, B, C, D, E, F, G) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7) = tuple
      product7(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E], t6.convert[F], t7.convert[G])
        .map(f.tupled)
        .flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, RR](f: (A, B, C, D, E, F, G) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7) = tuple
      product7(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E], t6.convert[F], t7.convert[G])
        .map(f.tupled)
        .flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, RL, RR](f: (A, B, C, D, E, F, G) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7) = tuple
      product7(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E], t6.convert[F], t7.convert[G])
        .map(f.tupled)
        .flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, RR](f: (A, B, C, D, E, F, G) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7) = tuple
      product7(t1.convert[A], t2.convert[B], t3.convert[C], t4.convert[D], t5.convert[E], t6.convert[F], t7.convert[G])
        .map(f.tupled)
    }
  }

  implicit class DynamicReprTuples8(
    tuple: Tuple8[DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr, DynamicRepr]
  ) {
    def applyT[A, B, C, D, E, F, G, H, RR](f: (A, B, C, D, E, F, G, H) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8) = tuple
      product8(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, RR](f: (A, B, C, D, E, F, G, H) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8) = tuple
      product8(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, RL, RR](f: (A, B, C, D, E, F, G, H) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8) = tuple
      product8(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, RR](f: (A, B, C, D, E, F, G, H) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8) = tuple
      product8(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples9(
    tuple: Tuple9[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, RR](f: (A, B, C, D, E, F, G, H, I) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9) = tuple
      product9(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, RR](f: (A, B, C, D, E, F, G, H, I) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9) = tuple
      product9(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, RL, RR](f: (A, B, C, D, E, F, G, H, I) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9) = tuple
      product9(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, RR](f: (A, B, C, D, E, F, G, H, I) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9) = tuple
      product9(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples10(
    tuple: Tuple10[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, RR](f: (A, B, C, D, E, F, G, H, I, J) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = tuple
      product10(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, RR](f: (A, B, C, D, E, F, G, H, I, J) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = tuple
      product10(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, RL, RR](f: (A, B, C, D, E, F, G, H, I, J) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = tuple
      product10(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, RR](f: (A, B, C, D, E, F, G, H, I, J) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = tuple
      product10(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples11(
    tuple: Tuple11[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, RR](f: (A, B, C, D, E, F, G, H, I, J, K) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = tuple
      product11(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, RR](f: (A, B, C, D, E, F, G, H, I, J, K) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = tuple
      product11(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, RL, RR](f: (A, B, C, D, E, F, G, H, I, J, K) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = tuple
      product11(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, RR](f: (A, B, C, D, E, F, G, H, I, J, K) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = tuple
      product11(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples12(
    tuple: Tuple12[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = tuple
      product12(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = tuple
      product12(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, RL, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = tuple
      product12(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = tuple
      product12(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples13(
    tuple: Tuple13[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = tuple
      product13(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = tuple
      product13(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, RL, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = tuple
      product13(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = tuple
      product13(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples14(
    tuple: Tuple14[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = tuple
      product14(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = tuple
      product14(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RL, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Either[RL, RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = tuple
      product14(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = tuple
      product14(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples15(
    tuple: Tuple15[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Try[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = tuple
      product15(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Option[RR])(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = tuple
      product15(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Either[RL, RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = tuple
      product15(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = tuple
      product15(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples16(
    tuple: Tuple16[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Try[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = tuple
      product16(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Option[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = tuple
      product16(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Either[RL, RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = tuple
      product16(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RR](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => RR)(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = tuple
      product16(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples17(
    tuple: Tuple17[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Try[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = tuple
      product17(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Option[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = tuple
      product17(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Either[RL, RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = tuple
      product17(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => RR
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = tuple
      product17(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples18(
    tuple: Tuple18[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Try[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = tuple
      product18(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Option[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = tuple
      product18(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Either[RL, RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = tuple
      product18(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => RR
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = tuple
      product18(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples19(
    tuple: Tuple19[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Try[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = tuple
      product19(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Option[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = tuple
      product19(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Either[RL, RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = tuple
      product19(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => RR
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = tuple
      product19(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples20(
    tuple: Tuple20[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Try[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = tuple
      product20(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Option[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = tuple
      product20(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Either[RL, RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = tuple
      product20(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => RR
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = tuple
      product20(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples21(
    tuple: Tuple21[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Try[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T],
      decU: Decode[U]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = tuple
      product21(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T],
        t21.convert[U]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Option[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T],
      decU: Decode[U]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = tuple
      product21(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T],
        t21.convert[U]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Either[RL, RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T],
      decU: Decode[U]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = tuple
      product21(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T],
        t21.convert[U]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => RR
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T],
      decU: Decode[U]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = tuple
      product21(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T],
        t21.convert[U]
      ).map(f.tupled)
    }
  }

  implicit class DynamicReprTuples22(
    tuple: Tuple22[
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr,
      DynamicRepr
    ]
  ) {
    def applyT[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Try[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T],
      decU: Decode[U],
      decV: Decode[V]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = tuple
      product22(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T],
        t21.convert[U],
        t22.convert[V]
      ).map(f.tupled).flatMap(_.toEither.left.map(t => WrappedError(t)))
    }

    def applyO[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Option[RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T],
      decU: Decode[U],
      decV: Decode[V]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = tuple
      product22(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T],
        t21.convert[U],
        t22.convert[V]
      ).map(f.tupled).flatMap(_.toRight(NilSmartConstructorError))
    }

    def applyE[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RL, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Either[RL, RR]
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T],
      decU: Decode[U],
      decV: Decode[V]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = tuple
      product22(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T],
        t21.convert[U],
        t22.convert[V]
      ).map(f.tupled).flatMap(_.left.map(WrappedError(_)))
    }

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RR](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => RR
    )(
      implicit decA: Decode[A],
      decB: Decode[B],
      decC: Decode[C],
      decD: Decode[D],
      decE: Decode[E],
      decF: Decode[F],
      decG: Decode[G],
      decH: Decode[H],
      decI: Decode[I],
      decJ: Decode[J],
      decK: Decode[K],
      decL: Decode[L],
      decM: Decode[M],
      decN: Decode[N],
      decO: Decode[O],
      decP: Decode[P],
      decQ: Decode[Q],
      decR: Decode[R],
      decS: Decode[S],
      decT: Decode[T],
      decU: Decode[U],
      decV: Decode[V]
    ): Result[RR] = {
      val (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = tuple
      product22(
        t1.convert[A],
        t2.convert[B],
        t3.convert[C],
        t4.convert[D],
        t5.convert[E],
        t6.convert[F],
        t7.convert[G],
        t8.convert[H],
        t9.convert[I],
        t10.convert[J],
        t11.convert[K],
        t12.convert[L],
        t13.convert[M],
        t14.convert[N],
        t15.convert[O],
        t16.convert[P],
        t17.convert[Q],
        t18.convert[R],
        t19.convert[S],
        t20.convert[T],
        t21.convert[U],
        t22.convert[V]
      ).map(f.tupled)
    }
  }

// $$COVERAGE-ON$$
  //  /end/producthelper/ - DO NOT REMOVE
}
