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

object ResultProducts {

  def product2[A, B](r1: Result[A], r2: Result[B]): Result[(A, B)] = (r1, r2) match {
    case (Right(v1), Right(v2)) => Right(v1, v2)
    case (Left(rl), Left(rr))   => Left(rl.combine(rr))
    case (Left(l), _)           => Left(l)
    case (_, Left(l))           => Left(l)
  }

  // /start/resulthelper/ - DO NOT REMOVE

  def product3[A, B, C](r1: Result[A], r2: Result[B], r3: Result[C]): Result[(A, B, C)] =
    product2(product2(r1, r2), r3) map {
      case ((x1, x2), x3) => (x1, x2, x3)
    }

  def product4[A, B, C, D](r1: Result[A], r2: Result[B], r3: Result[C], r4: Result[D]): Result[(A, B, C, D)] =
    product2(product3(r1, r2, r3), r4) map {
      case ((x1, x2, x3), x4) => (x1, x2, x3, x4)
    }

  def product5[A, B, C, D, E](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E]
  ): Result[(A, B, C, D, E)] =
    product2(product4(r1, r2, r3, r4), r5) map {
      case ((x1, x2, x3, x4), x5) => (x1, x2, x3, x4, x5)
    }

  def product6[A, B, C, D, E, F](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F]
  ): Result[(A, B, C, D, E, F)] =
    product2(product5(r1, r2, r3, r4, r5), r6) map {
      case ((x1, x2, x3, x4, x5), x6) => (x1, x2, x3, x4, x5, x6)
    }

  def product7[A, B, C, D, E, F, G](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G]
  ): Result[(A, B, C, D, E, F, G)] =
    product2(product6(r1, r2, r3, r4, r5, r6), r7) map {
      case ((x1, x2, x3, x4, x5, x6), x7) => (x1, x2, x3, x4, x5, x6, x7)
    }

  def product8[A, B, C, D, E, F, G, H](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H]
  ): Result[(A, B, C, D, E, F, G, H)] =
    product2(product7(r1, r2, r3, r4, r5, r6, r7), r8) map {
      case ((x1, x2, x3, x4, x5, x6, x7), x8) => (x1, x2, x3, x4, x5, x6, x7, x8)
    }

  def product9[A, B, C, D, E, F, G, H, I](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I]
  ): Result[(A, B, C, D, E, F, G, H, I)] =
    product2(product8(r1, r2, r3, r4, r5, r6, r7, r8), r9) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8), x9) => (x1, x2, x3, x4, x5, x6, x7, x8, x9)
    }

  def product10[A, B, C, D, E, F, G, H, I, J](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J]
  ): Result[(A, B, C, D, E, F, G, H, I, J)] =
    product2(product9(r1, r2, r3, r4, r5, r6, r7, r8, r9), r10) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), x10) => (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
    }

  def product11[A, B, C, D, E, F, G, H, I, J, K](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K)] =
    product2(product10(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10), r11) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), x11) => (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
    }

  def product12[A, B, C, D, E, F, G, H, I, J, K, L](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    product2(product11(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11), r12) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11), x12) => (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
    }

  def product13[A, B, C, D, E, F, G, H, I, J, K, L, M](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    product2(product12(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12), r13) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12), x13) => (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
    }

  def product14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    product2(product13(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13), r14) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13), x14) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
    }

  def product15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N],
    r15: Result[O]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    product2(product14(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14), r15) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14), x15) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
    }

  def product16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N],
    r15: Result[O],
    r16: Result[P]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    product2(product15(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15), r16) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15), x16) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
    }

  def product17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N],
    r15: Result[O],
    r16: Result[P],
    r17: Result[Q]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    product2(product16(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16), r17) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16), x17) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
    }

  def product18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N],
    r15: Result[O],
    r16: Result[P],
    r17: Result[Q],
    r18: Result[R]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    product2(product17(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17), r18) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17), x18) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
    }

  def product19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N],
    r15: Result[O],
    r16: Result[P],
    r17: Result[Q],
    r18: Result[R],
    r19: Result[S]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    product2(product18(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18), r19) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18), x19) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
    }

  def product20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N],
    r15: Result[O],
    r16: Result[P],
    r17: Result[Q],
    r18: Result[R],
    r19: Result[S],
    r20: Result[T]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    product2(product19(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19), r20) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19), x20) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)
    }

  def product21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N],
    r15: Result[O],
    r16: Result[P],
    r17: Result[Q],
    r18: Result[R],
    r19: Result[S],
    r20: Result[T],
    r21: Result[U]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    product2(product20(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20), r21) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20), x21) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)
    }

  def product22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
    r1: Result[A],
    r2: Result[B],
    r3: Result[C],
    r4: Result[D],
    r5: Result[E],
    r6: Result[F],
    r7: Result[G],
    r8: Result[H],
    r9: Result[I],
    r10: Result[J],
    r11: Result[K],
    r12: Result[L],
    r13: Result[M],
    r14: Result[N],
    r15: Result[O],
    r16: Result[P],
    r17: Result[Q],
    r18: Result[R],
    r19: Result[S],
    r20: Result[T],
    r21: Result[U],
    r22: Result[V]
  ): Result[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    product2(product21(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21), r22) map {
      case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21), x22) =>
        (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)
    }
  // /end/resulthelper/ - DO NOT REMOVE

}
