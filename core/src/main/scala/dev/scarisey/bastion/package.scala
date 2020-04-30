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

package dev.scarisey

package object bastion extends ProductHelper {
  type Result[T] = Either[DecodeError, T]

  implicit class ConverterSyntax[A](a: A) {
    def convert[B](implicit decode: Decode[B], encode: Encode[A]): Result[B] = decode.from(encode.to(a))
  }

  implicit class DynamicReprConverter(d: DynamicRepr) {
    def convert[A](implicit decode: Decode[A]): Result[A] = decode.from(d)
  }
}
