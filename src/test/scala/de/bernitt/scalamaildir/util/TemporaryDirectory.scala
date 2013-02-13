/*
 * Copyright 2013 Folker Bernitt
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

package de.bernitt.scalamaildir.util

import java.io.File
import org.scalatest.{Suite, BeforeAndAfterEach}

trait TemporaryDirectory extends BeforeAndAfterEach {
  this: Suite =>

  protected val tempDir = createTemporaryDirectory()

  override def beforeEach() {
    tempDir.mkdir()
    if (!tempDir.exists() || !tempDir.isDirectory)
      throw new RuntimeException("Failed to create temporary dir " + tempDir.getAbsolutePath)
    super.beforeEach()
  }

  override def afterEach() {
    deleteDirectory(tempDir)
    if (tempDir.exists()) throw new RuntimeException("Failed to delete temp dir " + tempDir.getAbsolutePath)
    super.afterEach()
  }

  private def createTemporaryDirectory(): File = {
    val file = File.createTempFile("test", "dir")
    file.delete()
    file
  }

  private def deleteDirectory(dir: File): Boolean = {
    if (dir.exists()) {
      dir.listFiles().foreach {
        path =>
          if (path.isDirectory) {
            deleteDirectory(path)
          } else {
            path.delete()
          }
      }
    }
    dir.delete()
  }
}
