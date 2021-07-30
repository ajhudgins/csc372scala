/** Aaron Hudgins (ajhudgins@email.arizona.edu)
 *  MazeSection.scala
 *  Final Project, Part 3: Maze Generator
 *  December 9, 2019
 *  CSc 372, Dr. McCann
 *
 *  This class represents a Maze section object. When a maze
 *  section is initialized, it will have all walls turned on
 *  by default. This is to be used in unison with the Maze
 *  class.
 *  @param x is an Integer location of the maze section.
 *  @param y is an Integer location of the maze section.
 */
class MazeSection (x : Int, y: Int) {
  private var _visited = false // For our generation method
  private var _edge = false

  private var _playerOn = " " // Player initially off

  private val _x = x
  private val _y = y

  private val _neighbors = new Array[MazeSection](4) // Neighbors array
  private val _walls = Array(true, true, true, true) // Walls array

  private val rand = scala.util.Random

  /** This function gets the x location of the maze
   *  section.
   *  @return an Integer that represents the x location
   *          of the section.
   */
  def xLoc: Int = _x

  /** This function gets the y location of the maze
   *  section.
   *  @return an Integer that represents the y location
   *          of the section.
   */
  def yLoc: Int = _y

  /** This function gets the north neighbor of the maze
   *  section.
   *  @return a MazeSection that is the north neighbor
   *          if this section.
   */
  def north: MazeSection = _neighbors(0)

  /** This function gets the south neighbor of the maze
   *  section.
   *  @return a MazeSection that is the south neighbor
   *          if this section.
   */
  def south: MazeSection = _neighbors(1)

  /** This function gets the east neighbor of the maze
   *  section.
   *  @return a MazeSection that is the east neighbor
   *          if this section.
   */
  def east:  MazeSection = _neighbors(2)

  /** This function gets the west neighbor of the maze
   *  section.
   *  @return a MazeSection that is the west neighbor
   *          if this section.
   */
  def west:  MazeSection = _neighbors(3)

  /** This function gets whether the north wall exists
   *  or not.
   *  @return if the north wall exists, return true,
   *          false otherwise.
   */
  def northWall: Boolean = _walls(0)

  /** This function gets whether the south wall exists
   *  or not.
   *  @return if the south wall exists, return true,
   *          false otherwise.
   */
  def southWall: Boolean = _walls(1)

  /** This function gets whether the east wall exists
   *  or not.
   *  @return if the east wall exists, return true,
   *          false otherwise.
   */
  def eastWall:  Boolean = _walls(2)

  /** This function gets whether the west wall exists
   *  or not.
   *  @return if the west wall exists, return true,
   *          false otherwise.
   */
  def westWall:  Boolean = _walls(3)

  /** This function will turn off the north wall. */
  def northWallSwitchOff(): Unit =_walls(0) = false

  /** This function will turn off the south wall. */
  def southWallSwitchOff(): Unit =_walls(1) = false

  /** This function will turn off the east wall. */
  def eastWallSwitchOff():  Unit =_walls(2) = false

  /** This function will turn off the west wall. */
  def westWallSwitchOff():  Unit =_walls(3) = false

  /** This function will set the maze sections north
   *  neighbor.
   *  @param section A different maze section to this one.
   */
  def north_(section: MazeSection): Unit =_neighbors(0) = section

  /** This function will set the maze sections south
   *  neighbor.
   *  @param section A different maze section to this one.
   */
  def south_(section: MazeSection): Unit =_neighbors(1) = section

  /** This function will set the maze sections east
   *  neighbor.
   *  @param section A different maze section to this one.
   */
  def east_ (section: MazeSection): Unit =_neighbors(2) = section

  /** This function will set the maze sections west
   *  neighbor.
   *  @param section A different maze section to this one.
   */
  def west_ (section: MazeSection): Unit =_neighbors(3) = section

  /** This function will get if the maze section has been
   *  visited or not.
   *  @return If visited, true, if not, false.
   */
  def visited(): Boolean = _visited

  /** This function will get if the maze section is an
   *  edge
   *  @return If an edge, true, if not, false.
   */
  def edge(): Boolean = _edge

  /** This function will mark the maze section as visited. */
  def markVisited(): Unit = _visited = true

  /** This function will mark the maze section as an edge. */
  def markAsEdge(): Unit = _edge = true

  /** This function will place the player on the section */
  def playerOn(): Unit = _playerOn = "x"

  /** This function will remove the player from the
   *  section
   */
  def playerOff(): Unit = _playerOn = " "

  /** This function will determine whether all of the maze
   *  section's neighbors have been visited or not.
   *  @return A boolean of true if all have been visited,
   *         false if not.
   */
  def allNeighborsVisited(): Boolean = {
    if((north == null || north.visited()) && (south == null || south.visited()) &&
      (east == null || east.visited()) && (west == null || west.visited())) {
      // We check to see if north, south, east, and west are null because if
      // they are, then we cannot visit them in the first place.
      return true
    }
    false
  }

  /** This function will randomly select a neighbor and
   *  return it.
   *  @return a MazeSection that has been randomly selected.
   */
  def pickNeighbor(): MazeSection = {
    var random = rand.nextInt(4) // Get a random number from 0-3
    var neighbor = _neighbors(random) // Get the neighbor

    while(neighbor == null || neighbor.visited()) {
      // While selected neighbor has been visited.
      random = rand.nextInt(4)
      neighbor = _neighbors(random)
    }
    neighbor
  }

  /** This method will create an array of strings that
   *  represent the maze section's walls. The north wall
   *  will be index 0 of the array. The east and west
   *  walls will be index 1 of the array. The south walls
   *  will be index 2 of the array. It will then return
   *  said array.
   *  @return an array of Strings that represent each of
   *          the section's walls.
   */
  def wallStrings(): Array[String] = {
    // +---+
    // |   |
    // +---+
    val wallStrings = Array("+---+", "|   |", "+---+")

    if(!northWall){
      // If north wall doesn't exist
      wallStrings(0) =  "+   +"
    }
    if(!southWall){
      // If south wall doesn't exist
      wallStrings(2) =  "+   +"
    }
    if(!eastWall && westWall){
      // If east wall doesn't exist, but west wall does
      wallStrings(1) = "  " + _playerOn + " |"

    } else if(eastWall && !westWall) {
      // If east wall does exist, but west wall does not
      wallStrings(1) = "| " + _playerOn + "  "

    } else if(!eastWall && !westWall) {
      // If both east wall and west wall don't exist
      wallStrings(1) = "  " + _playerOn + "  "

    } else {
      // If both east and west walls exist
      wallStrings(1) = "| " + _playerOn + " |"
    }
    wallStrings
  }
}