package org.project.euler

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/*
 * Project Euler (Problem 79) http://projecteuler.net/problem=79
 * 
 * A common security method used for online banking is to ask the user for three random characters from a passcode.
 * For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.
 * 
 * The text file, keylog.txt, contains fifty successful login attempts. Given that the three characters are always asked for in order,
 * analyse the file so as to determine the shortest possible secret passcode of unknown length.
 * 
 * Basically the idea is to build a precedence graph for all the keylog attempts and perform a topological sort.
 **/
case class GraphNode(nodeVal: Char, var outgoingEdges: Set[Char], var incomingEdges: Set[Char]) {
  def addReference(otherNode: GraphNode) = { outgoingEdges += otherNode.nodeVal; otherNode.incomingEdges += this.nodeVal }
  def removeReference(otherNode: GraphNode) = { incomingEdges -= otherNode.nodeVal; outgoingEdges -= otherNode.nodeVal; }
}

object graph {
  var vertices = Map.empty[Char, GraphNode]
  var vertices1 = Map.empty[Char, Int]

  def getVertex(vertex: Char): GraphNode = {
    if (!vertices.contains(vertex)) {
      vertices = vertices + ((vertex, GraphNode(vertex, Set.empty[Char], Set.empty[Char])))
    }
    vertices.get(vertex).get
  }

  def connect(node1: Char, node2: Char) = getVertex(node1).addReference(getVertex(node2))
  def disconnect(node1: Char, node2: Char) = {
    val nodeVal1 = getVertex(node1)
    val nodeVal2 = getVertex(node2)

    nodeVal1.removeReference(nodeVal2)
    nodeVal2.removeReference(nodeVal1)
  }
}

object PasswordDerivation {

  def main(args: Array[String]) {
    Source.fromFile("config/keylog.txt").getLines.foreach(f => buildGraph(f));
    Console.out.println(topologicalSort)
  }

  def buildGraph(str: String) = {
    str.indices.foreach(index => if (index < str.size - 1) {
      graph.connect(str.charAt(index), str.charAt(index + 1))
    })
  }

  def topologicalSort: Array[Char] = {
    val result = ArrayBuffer[Char]()
    var temp = Set.empty[Char]

    graph.vertices.values.foreach(p => if (p.incomingEdges.isEmpty) {
      temp = temp + p.nodeVal
    })

    while (!temp.isEmpty) {
      val currentNode = temp.iterator.next

      temp -= currentNode; //Remove the node from the buffer.
      result += currentNode; //Add this node to the result.
      val nodeVal = graph.getVertex(currentNode)

      //Update the graph to remove the edges from this node.
      nodeVal.outgoingEdges.foreach(p => { graph.disconnect(nodeVal.nodeVal, p) })

      graph.vertices.values.foreach(p => if (p.incomingEdges.isEmpty && !result.contains(p.nodeVal)) {
        temp = temp + p.nodeVal
      })
    }

    result toArray
  }
}