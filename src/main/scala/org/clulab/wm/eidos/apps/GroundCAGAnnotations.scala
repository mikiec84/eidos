package org.clulab.wm.eidos.apps

import java.io.{File, PrintWriter}

import ai.lum.common.ConfigUtils._
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.Processor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.groundings.{EidosOntologyGrounder, OntologyHandler}
import org.clulab.wm.eidos.utils.StopwordManager
import scala.io.{BufferedSource, Source}


object GroundCAGAnnotations extends App {

  // point to the right config
  val config: Config = ConfigFactory.load("eidos")

  // initialize an ontologyHandler
  val proc: Processor =  new FastNLPProcessor
  val stopwordManager: StopwordManager = StopwordManager.fromConfig(config)
  val ontologyHandler: OntologyHandler = OntologyHandler.load(config[Config]("ontologies"), proc, stopwordManager)

  val grounder: EidosOntologyGrounder = ontologyHandler.grounders.head


  // load tsv file from resources
  // TODO: make this loop over all the files, not just one at a time
  val sheetName = "UNHCR"
  val file: BufferedSource = Source.fromURL(getClass.getResource("/CAGlinksForGrounding/"+sheetName+"-CAG.tsv"))
  val fileString: String = file.mkString
  val lines: Array[String] = fileString.split("\n")
  //  file.close()

  var newFileArray = Array(lines.head)

  for (line <- lines.tail) {
//    var index = lines.indexOf(line).toString
    val splitLine = line.split("\t")

    //
    //  make sure indices are correct!
    //  some annotation files have the sentence moved to column 1 for easy reading, some do not
    //  this shifts all the indices!
    //

    // get cause/effect text
    val sourceText = splitLine(3)
    val targetText = splitLine(6)


    // get cause/effect nodes for grounding
    var sourceNode = splitLine(4)
    var targetNode = splitLine(7)

    // how many groundings to take
    val topk = 5

    // get the source groundings
    val sourceNodeGrounder = grounder.groundText(sourceText)
    val topSourceGroundings = sourceNodeGrounder.take(topk).toArray
    val readableSource = topSourceGroundings.map(gr => (gr._1.name, gr._2))

    // get the target groundings
    val targetNodeGrounder = grounder.groundText(targetText)
    val topTargetGroundings = targetNodeGrounder.take(topk).toArray
    val readableTarget = topTargetGroundings.map(gr => (gr._1.name, gr._2))

    sourceNode = readableSource.mkString(" ")
    targetNode = readableTarget.mkString(" ")

    val firstPart = splitLine.slice(0,4)
    val middlePart = splitLine.slice(5,7)
    val lastPart = splitLine.slice(8,splitLine.length)

    val newLine = firstPart ++ Array(sourceNode) ++ middlePart ++ Array(targetNode) ++ lastPart
    newFileArray = newFileArray :+ newLine.mkString("\t")
  }

  // makes new file with columns filled by source/target nodes; should be same file?
  val writer = new PrintWriter(new File("src/main/resources/groundedCAGsNewOntology/"+sheetName+"-CAG-NewOntology.tsv"))
  writer.write(newFileArray.mkString("\n"))
  writer.close()

}