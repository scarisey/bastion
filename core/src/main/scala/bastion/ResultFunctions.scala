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

import scala.reflect.ClassTag

object ResultFunctions {
  //imperative implementation for performance reasons ! see https://www.lihaoyi.com/post/MicrooptimizingyourScalacode.html
  def traverse[A:ClassTag, B](xs: Iterable[A])(f: A => Result[B]): Result[Iterable[B]] = {
//    val arraySource: Array[Any] = xs.toArray
//    val length                  = arraySource.size
//    val arrayTarget: Array[Any] = new Array[Any](length)
//    var i                       = 0
//    var acc: Result[Array[Any]] = Right(arrayTarget)
//    while (i < length) {
//      f(arraySource(i).asInstanceOf[A]).fold(err => acc = Left(err), b => arrayTarget(i) = b.asInstanceOf[Object])
//      i += 1
//    }
//    acc.asInstanceOf[Result[Array[B]]].map(_.toIterable)
    val zero: Result[List[B]] = Right(List.empty[B])
    xs.foldRight(zero) {
      case (x, acc) =>
        acc.flatMap(rs => f(x).map(r => r :: rs))
    }
  }
}
