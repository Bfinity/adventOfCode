


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps

/**
  * Created by bfinerocks on 12/7/16.
  */

@RunWith(classOf[JUnitRunner])
class Day1Test extends FunSuite {

  test("will convert string to char array"){
    val subject = new Day1()
    assert(subject.convertDirectionsToArray("R1").length == 1)
  }

  test("will move East when facing north and moving right"){
    val subject = new Day1()
    var directions = subject.convertDirectionsToArray("R1")
    var north = subject.North(0, 0)
    assert(subject.move(north, directions) == subject.East(1,0))
  }

/*  test("will be distance of 5 blocks from starting point"){
    val subject = new Day1()
    val directions= subject.convertDirectionsToArray("R2, L3")
    val start = subject.North(0, 0)
    assert(subject.distanceFromStartingLocation(start, subject.move(start, directions)) == 5)
  }

  test("will be distance of 2 blocks from starting point"){
    val subject = new Day1()
    val directions= subject.convertDirectionsToArray("R2, R2, R2")
    val start = subject.North(0, 0)
    assert(subject.distanceFromStartingLocation(start, subject.move(start, directions)) == 2)
  }

  test("will be distance of 12 blocks from starting point"){
    val subject = new Day1()
    val directions= subject.convertDirectionsToArray("R5, L5, R5, R3")
    val start = subject.North(0, 0)
    assert(subject.distanceFromStartingLocation(start, subject.move(start, directions)) == 12)
  }

  test("will be distance of 11 blocks from starting point"){
    val subject = new Day1()
    val directions= subject.convertDirectionsToArray("R5, L2, L1, R1, R3, R3, L3, R3, R4, L2, R4, L4, R4")
    val start = subject.North(0, 0)
    assert(subject.distanceFromStartingLocation(start, subject.move(start, directions)) == 11)
  }*/

  test("will be list of 3"){
    val subject = new Day1()
    val start = subject.North(0,0)
    val end = start.turn("R", 2)
    assert(subject.captureLocations(start, end).length == 3)
  }

  test("will create list"){
    val subject = new Day1()
    val start = subject.North(0,0)
    val test = List(subject.LocationsVisited(0,0),subject.LocationsVisited(1,0),subject.LocationsVisited(2,0))
    assert(subject.captureLocations(start, start.turn("R", 2)).reverse == test)
  }

  test("will create list of 3"){
    val subject = new Day1()
    val directions = subject.convertDirectionsToArray("R2")
    val start = subject.North(0, 0)
    assert(subject.trackPlacesVisited(start, directions, List()).length == 3)
  }

  test("will be list"){
    val subject = new Day1()
    val directions = subject.convertDirectionsToArray("R2, R1")
    val start = subject.North(0,0)
    val test = List(subject.LocationsVisited(0,0),subject.LocationsVisited(1,0),subject.LocationsVisited(2,0), subject.LocationsVisited(2, -1))
    assert(subject.trackPlacesVisited(start, directions, List()) == test)
  }

  test("first location will be 4, 0"){
    val subject = new Day1()
    val directions = subject.convertDirectionsToArray("R8, R4, R4, R8")
    val start = subject.North(0,0)
    val test = subject.LocationsVisited(4, 0)
    assert(subject.compareLocations(subject.trackPlacesVisited(start, directions, List())) == test)
  }

  test("first location will be 2, 0"){
    val subject = new Day1()
    val directions = subject.convertDirectionsToArray("R2, L1, R1, R1, R2")
    val start = subject.North(0,0)
    val test = subject.LocationsVisited(2, 0)
    assert(subject.compareLocations(subject.trackPlacesVisited(start, directions, List())) == test)
  }

  test("first location will be 5, 0"){
    val subject = new Day1()
    val directions = subject.convertDirectionsToArray("  R5, L2, L1, R1, R3, R3, L3, R3, R4, L2, R4, L4, R4, R3, L2, L1, L1, R2, R4, R4, L4, R3, L2, R1, L4, R1, R3, L5, L4, L5, R3, L3, L1, L1, R4, R2, R2, L1, L4, R191, R5, L2, R46, R3, L1, R74, L2, R2, R187, R3, R4, R1, L4, L4, L2, R4, L5, R4, R3, L2, L1, R3, R3, R3, R1, R1, L4, R4, R1, R5, R2, R1, R3, L4, L2, L2, R1, L3, R1, R3, L5, L3, R5, R3, R4, L1, R3, R2, R1, R2, L4, L1, L1, R3, L3, R4, L2, L4, L5, L5, L4, R2, R5, L4, R4, L2, R3, L4, L3, L5, R5, L4, L2, R3, R5, R5, L1, L4, R3, L1, R2, L5, L1, R4, L1, R5, R1, L4, L4, L4, R4, R3, L5, R1, L3, R4, R3, L2, L1, R1, R2, R2, R2, L1, L1, L2, L5, L3, L1")
    val start = subject.North(0,0)
    val test = subject.LocationsVisited(2, 0)
    assert(subject.compareLocations(subject.trackPlacesVisited(start, directions, List())) == test)
  }

  test("what the hell"){
    val subject = new Day1()
    val directions = "R2, L1, R1, R1, R2"
    val test = subject.LocationsVisited(2, 0)
    assert(subject.findFirstPlaceVisitedTwiceByString(directions) == test)
  }


}
