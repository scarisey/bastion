import java.nio.file.Paths

import org.scalafmt.interfaces.Scalafmt
import sbt.File
import sbt.IO

import scala.io.Source

/*
  Copy from https://github.com/zio/zio-config/blob/15fc359bf199af296dcd362a56882f1738fd5d9f/project/ProductBuilderCodeGen.scala
 */
object CodeGen {
  def readFile(filepath: File): List[String] = {
    val source = Source.fromFile(filepath.toString)
    try {
      source.getLines.toList
    } finally {
      source.close
    }
  }
  private def findLine(lines: List[String], marker: String, filepath: File): String =
    lines
      .find(_.contains(marker))
      .getOrElse(throw new RuntimeException(s"Cannot find marker $marker in file $filepath"))

  def replaceFileSection(
    filepath: File,
    marker: String,
    newContents: List[String],
    tempScalaFmtFile: File,
    scalaFmtPath: File
  ): Unit = {
    val scalafmt = Scalafmt.create(this.getClass.getClassLoader)

    val markerStart = s"/start/$marker/"
    val markerEnd   = s"/end/$marker/"
    val lines       = readFile(filepath)

    val markerStartLine = findLine(lines, markerStart, filepath)
    val markerEndLine   = findLine(lines, markerEnd, filepath)

    val beforeMarker = lines.takeWhile(_ != markerStartLine)
    val afterMarker  = lines.dropWhile(_ != markerEndLine).drop(1)

    val toWrite: List[String] = (beforeMarker :+ markerStartLine) ++ (newContents :+ markerEndLine) ++ afterMarker

    val result = IO.read(scalaFmtPath).replace("maxColumn = 120", "maxColumn = 12000")

    IO.write(tempScalaFmtFile, result)

    val formatted =
      scalafmt.format(tempScalaFmtFile.toPath, Paths.get("Main.scala"), toWrite.mkString("\n"))

    IO.write(filepath, formatted)
  }
}
