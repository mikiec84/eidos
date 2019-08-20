package org.clulab.wm.eidos.text.english

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import ai.lum.common.ConfigUtils._
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.Processor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.{EidosOntologyGrounder, OntologyHandler}
import org.clulab.wm.eidos.test.TestUtils.EnglishTest
import org.clulab.wm.eidos.utils.StopwordManager
import org.slf4j.{Logger, LoggerFactory}

import scala.io.{BufferedSource, Source}


//
//
//  THIS WILL BE PLUGGED IN SOMEWHERE ELSE LATER (PROBABLY APPS)
//  BUT IT IS HERE FOR NOW BECAUSE I KNOW HOW TO MAKE IT WORK HERE
//  IT IS NOT FINISHED YET!
//
//

class TestGroundCAGtemp extends EnglishTest{

  // point to the right config
  val config: Config = ConfigFactory.load("eidos")

  // initialize an ontologyHandler
  val proc: Processor =  new FastNLPProcessor
  val stopwordManager: StopwordManager = StopwordManager.fromConfig(config)
  val ontologyHandler: OntologyHandler = OntologyHandler.load(config[Config]("ontologies"), proc, stopwordManager)

  val grounder: EidosOntologyGrounder = ontologyHandler.grounders.head


  // load tsv file from resources
  val file: BufferedSource = Source.fromURL(getClass.getResource("/CAGlinksForGrounding/ReachJongleiJanCAG.tsv"))
  val fileString: String = file.mkString
  val lines: Array[String] = fileString.split("\n")
//  file.close()

  var newFileArray = Array(lines.head)

  for (line <- lines.tail) {
    var index = lines.indexOf(line).toString()
//    println("\n\nINDEX:\t" + index)

    var splitLine = line.split("\t")

    // get cause/effect text
    val sourceText = splitLine(4)
    val targetText = splitLine(7)


    // get cause/effect nodes for grounding
    var sourceNode = splitLine(5)
    var targetNode = splitLine(8)

    // decide how many groundings to take
    val topk = 5

    // get the source/target groundings
    val sourceNodeGrounder = grounder.groundText(sourceText)
    val topSourceGroundings = sourceNodeGrounder.take(topk).toArray

    val targetNodeGrounder = grounder.groundText(targetText)
    val topTargetGroundings = targetNodeGrounder.take(topk).toArray


//    println("\nSOURCE TEXT:\t" + sourceText)
//    println("SOURCE GROUNDINGS:")
    var justSourceGroundings = Array[String]()
    for (grounding <- topSourceGroundings) {
      justSourceGroundings = justSourceGroundings :+ grounding._1.toString
//      println(grounding._1) // only prints grounding, not score
    }

//    println("\nTARGET TEXT:\t" + targetText)
//    println("TARGET GROUNDINGS:")
    var justTargetGroundings = Array[String]()
    for (grounding <- topTargetGroundings) {
      justTargetGroundings = justTargetGroundings :+ grounding._1.toString
//      println(grounding._1) // only prints grounding, not score
    }

    sourceNode = justSourceGroundings.mkString(" ")
    targetNode = justTargetGroundings.mkString(" ")

    val firstPart = splitLine.slice(0,6)
    val middlePart = splitLine.slice(6,9)

//    val lastElement = splitLine.last//.indexOf()
//    println("LAST ELEMENT:\t" + lastElement)
    val lastPart = splitLine.slice(9,splitLine.length)


    val newLine = firstPart ++ Array(sourceNode) ++ middlePart ++ Array(targetNode) ++ lastPart
//    val newLine2 = newLine ++ middlePart
//    val newLine3 = newLine2 ++ Array(targetNode)
//    val newLine4 = newLine3 ++ lastPart

//    println("NEW LINE:\n" + newLine.mkString("\t"))
    newFileArray = newFileArray :+ newLine.mkString("\t")
  }

  println("NEW FILE ARRAY:\n" + newFileArray.mkString("\n"))

  // makes new file with columns filled by source/target nodes; should be same file?
  val writer = new PrintWriter(new File("groundedCAGlinks.tsv"))
  writer.write(newFileArray.mkString("\n"))
  writer.close()

}