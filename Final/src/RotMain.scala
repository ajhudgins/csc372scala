/** Aaron Hudgins (ajhudgins@email.arizona.edu)
 *  RotMain.scala
 *  Final Project, Part 2: ROT13
 *  November 25, 2019
 *  CSc 372, Dr. McCann
 *
 *  Information about Scala:
 *  https://docs.google.com/document/d/1nxuMBV1zyz6uUneUQ_ypFtj3K1lJpgSCHey0HTZqu38
 *
 *  This program is our main file for the ROT13 program.
 *  It will ask the user for an input text file, as well as the
 *  number they would like to ROT the contents of said file by.
 */
import java.io.PrintWriter
import scala.io.StdIn
import scala.io.Source
import scala.reflect.io.File

/** This object acts as our main function. It does
 *  this by extending the App class.
 */
object RotMain extends App {
  // Get file name
  print("File name? ")
  var fileName = StdIn.readLine()

  // Check if file exists
  if (!File(fileName).exists) {
    println("File not found, please try again.")
    sys.exit()
  }
  val lines = readFile(fileName) // Get the contents of the file

  // Attempt to get the ROT number
  var rotNum = 0
  print("ROT number? ")
  try {
    rotNum = StdIn.readInt()

  // If not an integer, quit
  } catch {
    case x : NumberFormatException => {
    println("Invalid number entered.")
      sys.exit()
    }
  }

  // ROT the contents of the file
  var rot = new ROT(rotNum)
  var rotS = rot.rotString(lines)

  // Write to out ROTed contents
  val newFileName = fileName + ".rot-" + rotNum
  writeFile(rotS, newFileName)

  /** This function takes a file name, and gets the contents
   *  of said file.
   * @param s is a string that represents a file name.
   * @return a string of containing all lines from the file.
   */
  def readFile(s: String): String = {
    var lines = ""

    // Iterate over lines in file
    for (line <- Source.fromFile(s).getLines) {
      lines += line + "\n" // Concatenate lines
    }
    return lines
  }

  /** This function will take the ROTed file contents, and
   * write them out to a specified file.
   * @param lines is a string of ROTed lines.
   * @param fileName is the file name where the contents will
   *                 be written to.
   */
  def writeFile(lines: String, fileName : String): Unit = {
    val writer = new PrintWriter(new java.io.File(fileName))
    writer.write(lines) // Write lines
    writer.close()
  }
}