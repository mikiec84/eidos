package org.clulab.wm.eidos.apps

import org.clulab.processors.Processor
import org.clulab.wm.eidos.utils.FileUtils
import scala.collection.mutable.ArrayBuffer
import org.clulab.processors.fastnlp.FastNLPProcessor


object ConceptAnalysis extends App {

  // load tsv file from resources
  val sheetName1 = "54660"
  val sheetName2 = "62801"
  val sheetName3 = "62803"
  val sheetName4 = "63604"
  val sheetName5 = "grounded"
  val sheetName6 = "ReachJonglei"

  // TODO: make this loop through directory rather than load all files manually
  val file1 = FileUtils.getTextFromResource("/groundingsWithScores/"+sheetName1+"-CAG-grounded.tsv").split("\n")
  val file2 = FileUtils.getTextFromResource("/groundingsWithScores/"+sheetName2+"-CAG-grounded.tsv").split("\n")
  val file3 = FileUtils.getTextFromResource("/groundingsWithScores/"+sheetName3+"-CAG-grounded.tsv").split("\n")
  val file4 = FileUtils.getTextFromResource("/groundingsWithScores/"+sheetName4+"-CAG-grounded.tsv").split("\n")
  val file5 = FileUtils.getTextFromResource("/groundingsWithScores/"+sheetName5+"-CAG-grounded.tsv").split("\n")
  val file6 = FileUtils.getTextFromResource("/groundingsWithScores/"+sheetName6+"-CAG-grounded.tsv").split("\n")

  val lines = file1 ++ file2 ++ file3 ++ file4 ++ file5 ++ file6
  println(lines.length)

  val proc:Processor = new FastNLPProcessor()

  val zeroText = new ArrayBuffer[String]
  for (line <- lines){
    val fields = line.split("\t")
    if (fields.length >= 9) {
//      println(line)
      // cause
      if (fields(5).trim == "0") {
        // add the cause text
        zeroText.append(fields(6))
      }
      if (fields(8).trim == "0") {
        zeroText.append(fields(9))
      }
    }
    else {
      println(s"LOST LINE: $line")
    }
  }

  val zeroTextGrouped = zeroText.groupBy(identity).mapValues(_.size)

  val zeroTextGroupedWords = zeroText.mkString(" ").split(" ").groupBy(identity).mapValues(_.length)

  val sortedList = zeroTextGrouped.toSeq.sortBy(-_._2)

  sortedList.foreach(tup => println(tup))

  println("\n")

  val sortedListWords = zeroTextGroupedWords.toSeq.sortBy(- _._2)

  sortedListWords.foreach(tup => println(tup))

}
