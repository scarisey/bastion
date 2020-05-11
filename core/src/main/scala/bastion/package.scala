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

package object bastion extends DynamicReprTuples {
  type Result[T] = Either[DecodeError, T]

  implicit class ConverterSyntax[A](a: A) {

    /**
     * Convert a type A to a type B.
     * An Encode[A] and a Decode[B] must be in implicit scope, as well as a Configuration.
     * Successful conversion will return Right(B), whether failing to convert will return a Left(DecodeError).
     * @see [[Configuration]], [[Encode]], [[Decode]], [[DecodeError]]
     */
    def convert[B](implicit decode: Decode[B], encode: Encode[A]): Result[B] = decode.from(encode.to(a))
  }

  implicit class DynamicReprConverter(d: DynamicRepr) {

    /**
     * Convert a DynamicRepr to a type B.
     * A Decode[B] must be in implicit scope.
     * Successful conversion will return Right(B), whether failing to convert will return a Left(DecodeError).
     * @see [[Decode]], [[DecodeError]], [[DynamicRepr]]
     */
    def convert[A](implicit decode: Decode[A]): Result[A] = decode.from(d)
  }

  implicit class IterableOps[A](xs: Iterable[A]) {
    def traverse[B](f: A => Result[B]): Result[List[B]] =
      ResultFunctions.traverse(xs)(f)
  }
}
