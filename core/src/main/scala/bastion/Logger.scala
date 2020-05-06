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

import java.util.logging.Level

// $COVERAGE-OFF$Utility class for internal use only
private[bastion] object Logger {
  private val rootLogger = java.util.logging.Logger.getLogger("")
  private val logger     = java.util.logging.Logger.getLogger("bastion")

  def setLevel(level: Level): Unit = {
    logger.info(s"log level set to $level")
    rootLogger.setLevel(level)
    rootLogger.getHandlers.foreach(h => h.setLevel(level))
  }

  def debug(message: String): Unit = logger.fine(message)
  def info(message: String): Unit  = logger.info(message)
}
// $COVERAGE-ON$
