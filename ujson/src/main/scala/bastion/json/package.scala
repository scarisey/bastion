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

import bastion.json.JsonDecoder.parse
import ujson.Readable

package object json {

  /**
   * Decode a JSON using uJson and Bastion. The input must be [[Readable]].
   */
  def decodeJson[T: Decoder](t: Readable): Result[T] = parse(t).convert[T]

  /**
   * Encode the input T to String, using uJson.
   */
  def encodeJson[T](t: T)(implicit encode: BasicJsonEncoder[T]): String = encode.write(t)

  implicit class EncodeToJson[A](a: A) {
    def asJson(implicit encode: BasicJsonEncoder[A]): String = encode.write(a)
  }
}
