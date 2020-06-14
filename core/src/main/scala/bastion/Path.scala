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
import bastion.Path._

import scala.collection.immutable.Queue
import scala.language.dynamics

final case class Path(queue: Queue[PathElem] = Queue()) extends Dynamic {

  def selectDynamic(field: String): Path = Path(queue.enqueue(Field(field)))

  def item(index: Int): Path = Path(queue.enqueue(SelectItem(index)))

  lazy val path: List[String] = queue.foldRight(List.empty[String]) {
    case (Root, acc)              => "root" :: acc
    case (Field(field), acc)      => field :: acc
    case (SelectItem(index), acc) => s"[$index]" :: acc
  }

  lazy val show: String = path.mkString("root.", ".", "")

}

object Path {

  def root: Path = Path(Queue(Root))
  sealed trait PathElem
  case object Root                        extends PathElem
  final case class Field(field: String)   extends PathElem
  final case class SelectItem(index: Int) extends PathElem
}
