/**
  * Created by bfinerocks on 12/7/16.
  */
class Day1 {

  abstract class CardinalDirection(x: Int, y: Int){
    val horizontal = x
    val vertical = y

    def turn(s:String, b: Int) : CardinalDirection
  }
  case class North(x: Int, y: Int) extends CardinalDirection(x,y){
    override def turn(turn: String, blocks: Int): CardinalDirection = turn match{
      case "R" => East(x+blocks, y)
      case "L" => West(x-blocks, y)
    }
  }
  case class East(x: Int, y: Int) extends CardinalDirection(x,y){
    override def turn(turn: String, blocks: Int): CardinalDirection = turn match{
      case "R" => South(x, y-blocks)
      case "L" => North(x, y+blocks)
    }
  }
  case class South(x: Int, y: Int) extends CardinalDirection(x,y){
    override def turn(turn: String, blocks: Int): CardinalDirection = turn match{
      case "R" => West(x-blocks, y)
      case "L" => East(x+blocks, y)
    }
  }
  case class West(x: Int, y: Int) extends CardinalDirection(x,y){
    override def turn(turn: String, blocks: Int): CardinalDirection = turn match{
      case "R" => North(x, y+blocks)
      case "L" => South(x, y-blocks)
    }
  }

  def getUpdatedCardinal(current: CardinalDirection, x: Int, y: Int): CardinalDirection = current match{
    case North(_, _) => North(x, y)
    case South(_, _) => South(x, y)
    case East(_, _) => East(x, y)
    case West(_, _) => West(x, y)
  }

  case class Directions(s: String){
    val look = s.substring(0,1)
    val walk = s.substring(1).toInt
  }

  abstract class Location

  case class LocationsVisited(x: Int, y: Int) extends Location{
    val places = Tuple2(x, y)
  }

  case class NilLocation() extends Location

  def findSolutionForDay1(directionsString: String): Int ={
    val start = North(0,0)
    distanceFromStartingLocation(start, move(start, convertDirectionsToArray(directionsString)))
  }

  def convertDirectionsToArray(directionsString: String): List[Directions] ={
    val directionsList = directionsString.replaceAll(" ", "").split(",").toList
    stringToDirection(directionsList, List())
  }

  private def stringToDirection(array: List[String], directions: List[Directions]) : List[Directions] = array match {
    case List() => directions
    case a :: yy => List(Directions(a)) ::: stringToDirection(yy, directions)
  }

  def move(currentDirection: CardinalDirection, directions: List[Directions]): CardinalDirection = directions match {
    case List() => currentDirection
    case a :: yy => move(currentDirection.turn(a.look, a.walk), yy)
  }

  def distanceFromStartingLocation(startingPoint: CardinalDirection, currentPoint: CardinalDirection): Int ={
    Math.abs((startingPoint.horizontal + currentPoint.horizontal)) + Math.abs((startingPoint.vertical + currentPoint.vertical))
  }

  def trackPlacesVisitedFromStart(start: CardinalDirection, steps: List[Directions], list: List[LocationsVisited]): List[LocationsVisited] ={
    trackPlacesVisited(start, steps, list).sortBy(l => l.x)
  }

  def trackPlacesVisited(prev: CardinalDirection, steps: List[Directions], list: List[LocationsVisited]): List[LocationsVisited] = steps match{
    case List() => list
    case a :: yy => captureLocations(prev, prev.turn(a.look, a.walk)) ::: trackPlacesVisited(prev.turn(a.look, a.walk), yy, list)
  }

  def captureLocations(prev: CardinalDirection, current: CardinalDirection): List[LocationsVisited] = {
    if(prev.horizontal == current.horizontal) captureVertical(current.horizontal, current.vertical, List())
    else captureHorizontal(current.horizontal, current.vertical, List())
  }

  def captureVertical(x: Int, y: Int, list: List[LocationsVisited]): List[LocationsVisited] = y match{
    case 0 => list ::: List(LocationsVisited(x,y))
    case _ => List(LocationsVisited(x, y)) ::: (if(y>0) captureVertical(x, y-1, list) else captureVertical(x, y+1, list))
  }

  def captureHorizontal(x: Int, y: Int, list: List[LocationsVisited]): List[LocationsVisited] = x match{
    case 0 => list ::: List(LocationsVisited(x,y))
    case x => List(LocationsVisited(x, y)) ::: (if(x > 0) captureHorizontal(x-1, y, list) else captureHorizontal(x+1, y, list))
  }

  def findFirstPlaceVisitedTwice(placesVisited: List[LocationsVisited]): Location ={
    compareLocations(placesVisited.head, placesVisited.tail)
  }

  def compareLocations(location: LocationsVisited, list: List[LocationsVisited]): Location = list match {
    case List() => NilLocation()
    case a :: yy => if(list.contains(location)) location else compareLocations(a, yy)
  }


}
