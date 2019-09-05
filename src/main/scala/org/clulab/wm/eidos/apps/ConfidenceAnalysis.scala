package org.clulab.wm.eidos.apps

import java.io.PrintWriter
import org.clulab.wm.eidos.utils.FileUtils


object ConfidenceAnalysis extends App {

  val filesList = FileUtils.findFiles("src/main/resources/confidenceTSVs", ".tsv")
  val outFilename = "scores-ALLfiles.tsv"
  val pw = new PrintWriter(outFilename)

  val header = "FACTOR" + "\t" +
    "FACTOR TEXT" + "\t" +
    "SCORE" + "\t" +
    "CONFIDENCE" + "\t" +
    "SOURCE FILE"

  pw.println(header)

  // loop over each file in the directory
  for (file <- filesList) {

    val fileTextArray = FileUtils.getTextFromFile(file).split("\n").tail  // ignore header line

    // loop through each line of the file
    for (line <- fileTextArray) {

      val lineSplit = line.split("\t")

      val factorAscore = lineSplit(1)
      val factorAtext = lineSplit(2)
      val factorAconfidence = lineSplit(3).split(",").last.split("\\)").head // funky formatting
      // TODO: decide definitively which confidence score to use; only rank 1, or use rank 2 if score=2, etc

      val factorBscore = lineSplit(8)
      val factorBtext = lineSplit(9)
      val factorBconfidence = lineSplit(5).split(",").last.split("\\)").head // funky formatting

      val rowA = "A" + "\t" + factorAtext + "\t" + factorAscore + "\t" + factorAconfidence + "\t" + file + "\n"
      val rowB = "B" + "\t" + factorBtext + "\t" + factorBscore + "\t" + factorBconfidence + "\t" + file + "\n"

      // write lines only if they have an annotator score
      if (factorAscore.length>0) {
        pw.write(rowA)
      }
      if (factorBscore.length>0) {
        pw.write(rowB)
      }
    }
  }
  pw.close()
}
