/** Aaron Hudgins (ajhudgins@email.arizona.edu)
 *  ROT.scala
 *  Final Project, Part 2: ROT13
 *  November 25, 2019
 *  CSc 372, Dr. McCann
 *
 *  Information about Scala:
 *  https://docs.google.com/document/d/1nxuMBV1zyz6uUneUQ_ypFtj3K1lJpgSCHey0HTZqu38
 *
 *  This class will allow us to create an instance
 *  of ROT, with a passed in Integer that represents
 *  how much we want to ROT a string by.
 *  @param _n is an integer that will tell the class
 *            how much to ROT by.
 */
class ROT (_n : Int) {
  private val n = _n % 26 // Set our ROT value

  /** This function gets the ROT value.
   *  @return an integer that represents the ROT value.
   */
  def getValue(): Int = {
    return n
  }

  /** This function will take a string, and ROTify it to the
   *  specified ROT value.
   *  @param s is a string that the user wants to be ROTified.
   *  @return a ROTified string.
   */
  def rotString(s : String): String = {
    // Apply the function, getAscii to whole string.
    var rotS = s.map(c => getAscii(c).toChar)
    return rotS
  }

  /** This function gets the ROTified ASCII value of a
   *  passed in Character
   *  @param c is a character to be ROTified.
   *  @return the ROTified char as a byte.
   */
  private def getAscii(c : Char): Byte = {
    val charAscii = c.toByte
    val charRot = rotAscii(charAscii) // ROT c
    return charRot
  }

  /** This function takes a byte and adds the ROT value to
   *  the byte. It will check if the byte is a ROTable char
   *  and if will also check if that ROTable char is upper or
   *  lower case.
   *  @param b is a byte.
   *  @return If the byte was ROTable, the ROT version of the
   *          byte. If not, the original byte.
   */
  private def rotAscii(b : Byte): Byte = {
    var upper = true

    // If upper case character
    if(b <= 90 && b >= 65) {
      var upper = true

    // If lower case character
    } else if (b <= 122 && b >= 97 ){
      upper = false

    // If not ROTable
    }  else {
      return b
    }
    var newB = b + n // ROT byte
    if(upper) {
      // If went over bound limit
      if(newB > 90) {
        var toAdd = newB % 90
        newB = 64 + toAdd

      // If went under bound limit
      } else if (newB < 65) {
        var toSub = 65 % newB
        newB = 91  - toSub
      }

    } else {
      // Over bound limit
      if(newB > 122) {
        var toAdd = newB % 122
        newB = 96 + toAdd

      // Under bound limit
      } else if (newB < 97) {
        var toSub = 97 % newB
        newB = 123 - toSub
      }
    }
    return newB.toByte
  }
}