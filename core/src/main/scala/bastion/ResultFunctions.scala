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

object ResultFunctions {
  def traverse[A, B](xs: Iterable[A])(f: A => Result[B]): Result[List[B]] = {
    val zero: Result[List[B]] = Right(List.empty[B])
    xs.foldLeft(zero) {
      case (acc, x) =>
        acc.flatMap(rs => f(x).map(r => rs :+ r))
    }
  }
}
