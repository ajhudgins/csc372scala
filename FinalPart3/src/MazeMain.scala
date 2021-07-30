/** Aaron Hudgins (ajhudgins@email.arizona.edu)
 *  MazeMain.scala
 *  Final Project, Part 3: Maze Generator
 *  December 9, 2019
 *  CSc 372, Dr. McCann
 *
 *  This program, when run will generate a random maze, and
 *  allow the user to traverse the maze using standard input
 *  from the keyboard. The user can traverse the maze by typing
 *  w, a, s, or d, followed by enter. The program will end when
 *  the user reaches the end of the maze.
 */
object MazeMain extends App {
  var maze = new Maze(10,10) // Create random maze
  println(maze)

  // While user has not reached end.
  while(!maze.gameOver) {
    print("Move? ")
    var in = scala.io.StdIn.readLine() // Get user input
    in = in.toLowerCase()

    if(in == "w") {
      // If user wants to move up
      if (maze.validMove("up")) {
          // If valid move, move up
        maze.moveUp()
        println(maze)
      }

    } else if(in == "s") {
      // If user wants to move down
      if (maze.validMove("down")) {
        // If valid move, move down
        maze.moveDown()
        println(maze)
      }

    } else if(in == "d") {
      // If user wants to move right
      if (maze.validMove("right")) {
        // If valid move, move right
        maze.moveRight()
        println(maze)
      }

    } else if(in == "a") {
      // If user wants to move left
      if (maze.validMove("left")) {
        // If valid move, move left
        maze.moveLeft()
        println(maze)
      }
    }
  }
  println("Congrats, you reached the end!") // End
}









