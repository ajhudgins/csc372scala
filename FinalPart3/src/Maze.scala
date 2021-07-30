/** Aaron Hudgins (ajhudgins@email.arizona.edu)
 *  Maze.scala
 *  Final Project, Part 3: Maze Generator
 *  December 9, 2019
 *  CSc 372, Dr. McCann
 *
 *  This class represents a Maze object. When a new maze is
 *  initialized, the class will generate a random maze using
 *  recursive backtracking. It utilizes an 2-D array of
 *  MazeSections to keep track of each section of the maze.
 *  @param w is an Integer that represents the width of the maze.
 *  @param h is an Integer that represents the height of the maze.
 */
class Maze(w: Int, h: Int) {
  private val _w = w // Width
  private val _h = h // Height
  private val _maze = Array.ofDim[MazeSection](_w,_h) // Keep track of sections

  // Initialize the maze, and set the neighbors of each section
  initializeMaze()
  setNeighbors()

  private var curr = mazeAt(0,0) // Section user is on
  private val end = mazeAt(w - 1, h - 1) // End of maze

  // Turn off walls for begin and end sections, and place player
  curr.eastWallSwitchOff()
  end.southWallSwitchOff()
  curr.playerOn()

  generateMaze(mazeAt(0,0)) // Generate random maze

  /** This function gets the width of the maze.
   *  @return An integer representing the width of the maze.
   */
  def width: Int = _w

  /** This function gets the height of the maze.
   *  @return An integer representing the height of the maze.
   */
  def height: Int = _h

  /** This function gets the maze representation.
   *  @return A 2-D array of Maze sections.
   */
  def maze: Array[Array[MazeSection]] = _maze

  /** This function gets the Maze Section at a specified
   *  location.
   *  @param x is an Integer that represents the x coordinate
   *           of the maze section we want.
   *  @param y is an Integer that represents the y coordinate
   *           of the maze section we want.
   *  @return A maze section.
   */
  def mazeAt(x: Int, y: Int): MazeSection = _maze(x)(y)

  /** This private function will initialize the maze when
   *  it is called. It will set each coordinate of the maze
   *  to a new maze section. It will also determine if the
   *  current section is an edge, and will mark it as an edge
   *  if so.
   */
  private def initializeMaze(): Unit = {
    for(i <- 0 until _w) {
      // Iterate from 0 until width
      for(j <- 0 until _h) {
        // Iterate from 0 until height
        val section = new MazeSection(i,j)

        if(i == 0 || i == _w - 1 || j == 0 || j == _h - 1) {
          // If the current maze section is an edge
          section.markAsEdge()
        }
        _maze(i)(j) = new MazeSection(i,j) // Set maze[i][j]
      }
    }
  }

  /** This private method, when called, will iterate through
   *  the maze, and set each maze section's neighbors.
   */
  private def setNeighbors(): Unit = {
    for(i <- 0 until _w) {
      for(j <- 0 until _h) {

        val section = _maze(i)(j) // Select maze section

        if(i > 0){
          // Check to see if in array bounds
          section.north_(_maze(i-1)(j))
        }
        if(i < _w - 1) {
          // Check to see if in array bounds
          section.south_(_maze(i+1)(j))
        }
        if(j > 0){
          // Check to see if in array bounds
          section.east_(_maze(i)(j-1))
        }
        if(j < _h - 1){
          // Check to see if in array bounds
          section.west_(_maze(i)(j+1))
        }
      }
    }
  }

  /** This private method, when called, will randomly
   *  generate the maze. It uses recurse backtracking to
   *  accomplish this goal.
   *  @param curr is the currently selected maze section.
   */
  private def generateMaze(curr: MazeSection): Unit = {
    curr.markVisited() // Mark curr as visited
    while(!curr.allNeighborsVisited()) {
      // While curr has unvisited neighbors
      val pick = curr.pickNeighbor() // Pick random neighbor

      if(curr.east == pick) {
        // If east neighbor selected,
        // switch off walls between curr and neighbor
        curr.eastWallSwitchOff()
        pick.westWallSwitchOff()

      } else if(curr.west == pick){
        // If west neighbor selected
        // switch off walls between curr and neighbor
        curr.westWallSwitchOff()
        pick.eastWallSwitchOff()

      } else if(curr.north == pick) {
        // If north neighbor selected
        // switch off walls between curr and neighbor
        curr.northWallSwitchOff()
        pick.southWallSwitchOff()

      } else {
        // If south neighbor selected
        // switch off walls between curr and neighbor
        curr.southWallSwitchOff()
        pick.northWallSwitchOff()
      }
      generateMaze(pick) // Recurse on picked neighbor
    }
  }

  /** This method overrides the toString object method.
   *  It will create a string representation of the maze,
   *  and return it.
   *  @return A string representation of the maze in its
   *          current state.
   */
  override def toString: String = {
    var mazeString = ""

    for(i <- 0 until _w) {
      for(j <- 0 until _h) {

        val string = mazeAt(i,j).wallStrings()(0) // Get north will string

        if(j > 0 ) {
          // If not first section in row, add sub string
          mazeString += string.substring(1,string.length)
        } else {
          // If first section
          mazeString += string
        }
      }
      mazeString += "\n"

      for(j <- 0  until _h) {

        val string = mazeAt(i,j).wallStrings()(1) // Get east and west wall strings
        if(j > 0 ) {
          // If not first section in row, add sub string
          mazeString += string.substring(1,string.length)
        } else {
          // If first section
          mazeString += string
        }
      }
      mazeString += "\n"
    }
    // Add south wall strings
    for(j <- 0 until _h) {

      val string = mazeAt(_w-1,j).wallStrings()(2) // get south wall string
      if(j > 0 ) {
        // If not first section in row, add sub string
        mazeString += string.substring(1,string.length)
      } else {
        // If first section
        mazeString += string
      }
    }
    mazeString
  }

  /** This function takes a string representation of a
   *  desired move, and checks to see if that move is a
   *  valid move or not.
   *  @param move A string that says "left", "right", "up", or
   *              "down".
   *  @return A boolean of true if valid move, false if not.
   */
  def validMove(move: String): Boolean = {
    if(curr.xLoc == 0 && curr.yLoc == 0 && move == "left") {
      // If on first section, and trying to move left
      return false
    }
    if(move == "up") {
      // If desired move is up
      if(curr.northWall) {
        // If north wall exists
        return false
      }
      true

    } else if(move == "down") {
      // If desired move is down
      if(curr.southWall) {
        // If south wall exists
        return false
      }
      true

    } else if(move == "left") {
      // If desired move is left
      if(curr.eastWall) {
        // if east wall exists
        return false
      }
      true

    }else {
      // If desired move is right
      if(curr.westWall) {
        // iff west wall exists
        return false
      }
      true
    }
  }

  /** This function moves the player upwards in
   *  the maze.
   */
  def moveUp(): Unit = {
    curr.playerOff() // Remove player from curr
    curr = curr.north // Set curr
    curr.playerOn() // Place player on curr
  }

  /** This function moves the player downwards in
   *  the maze.
   */
  def moveDown(): Unit = {
    curr.playerOff()
    curr = curr.south
    curr.playerOn()
  }

  /** This function moves the player left in
   *  the maze.
   */
  def moveLeft(): Unit = {
    curr.playerOff()
    curr = curr.east
    curr.playerOn()
  }

  /** This function moves the player right in
   *  the maze.
   */
  def moveRight(): Unit = {
    curr.playerOff()
    curr = curr.west
    curr.playerOn()
  }

  /** This function determines if the game is over.
   * @return A boolean of true if game is over, false
   *         if not.
   */
  def gameOver(): Boolean = {
    curr == end
  }
}