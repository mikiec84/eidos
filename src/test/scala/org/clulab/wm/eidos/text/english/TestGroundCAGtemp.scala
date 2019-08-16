package org.clulab.wm.eidos.text.english

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

  for (line <- lines.tail) {
    var index = lines.indexOf(line).toString()
    println("\n\nINDEX:\t" + index)

    var splitLine = line.split("\t")

    // get cause/effect text
    val sourceText = splitLine(4)
    val targetText = splitLine(7)


    // get cause/effect nodes for grounding
    val sourceNode = splitLine(5)
    val targetNode = splitLine(8)

    // decide how many groundings to take
    val topk = 5

    // get the source/target groundings
    val sourceNodeGrounder = grounder.groundText(sourceText)
    val topSourceGroundings = sourceNodeGrounder.take(topk).toArray

    val targetNodeGrounder = grounder.groundText(targetText)
    val topTargetGroundings = targetNodeGrounder.take(topk).toArray


    println("\nSOURCE TEXT:\t" + sourceText)
    println("SOURCE GROUNDINGS:")
    for (grounding <- topSourceGroundings) {
      println(grounding._1) // only prints grounding, not score
    }

    println("\nTARGET TEXT:\t" + targetText)
    println("TARGET GROUNDINGS:")
    for (grounding <- topTargetGroundings) {
      println(grounding._1) // only prints grounding, not score
    }
  }


}