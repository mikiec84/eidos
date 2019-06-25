package org.clulab.wm.eidos.actions

import org.clulab.odin._
import org.clulab.struct.Interval
import org.clulab.odin.{EventMention, Mention, State, TextBoundMention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.{Location, Time}
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.{EidosDocument, TimEx}
import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.processors.Document
import org.clulab.wm.eidos.{EidosActions, EidosSystem}
import org.clulab.wm.eidos.EidosActions.COREF_DETERMINERS
import org.clulab.wm.eidos.mentions.CrossSentenceEventMention
import org.clulab.wm.eidos.utils.{DisplayUtils, FileUtils}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object MigrationUtils {

  def processMigrationEvents(mentions: Seq[Mention]): Seq[Mention] = {
    // partition to get the migration events
    val (migrationEvents, other) = mentions.partition(_ matches EidosSystem.MIGRATION_LABEL)
    val handled = attachGeoLoc(migrationEvents)
    // todo: backoff times and locations -- use the normalization apis
    // todo: combine times (timeStart/timeEnd)????
    // todo: aggregation of cross-sentence stuff?????????????

    // return all
//        handled ++ other
//    assembleFragments(handled) ++ other
//    assembleMoreSpecific(assembleFragments(handled)) ++ other
    assembleTime(assembleFragments(handled)) ++ other

  }

  //
  def assembleTime(mentions: Seq[Mention]): Seq[Mention] = {
    //find attachments for each time mention (rel args = "time", "timeStart", "timeEnd"  in the hme
    //make interval from timeStart/timeEnd
    val (mentionsWithTime, other) = mentions.partition(_.arguments.exists(arg => arg._2.exists(tbm => tbm.label matches "Time")))

    for (m <- mentionsWithTime) println("need time attachments: " + m.text)

    val relArgs = Array("timeStart", "time", "timeEnd") //time-related args in migration events

    val mentionsWithTimeAttachments = attachTime(mentionsWithTime, relArgs)


    mentionsWithTimeAttachments ++ other

  }


  def combineStartEndTimes(mentions: Seq[Mention]): Seq[Mention] = {
    //assume the mentions do have time attachments
    ???

  }

  //Becky's stub for merging times
  def mergeTimes(ms: Seq[Mention], state: State): Mention = {
    val m = ms.head
    // get the overlapping
    val overlapping = state.mentionsFor(m.sentence, m.tokenInterval)
    ???
  }


  def attachTime(mentions: Seq[Mention], relArgs: Array[String]): Seq[Mention] = {
    //attaching TIMEX to time-related mentions witht the overlapping span
    //assume the mentions have time-related arguments

    val handled = for {
      m <- mentions
      times = m.document.asInstanceOf[EidosDocument].times
      oldArgs = for {
        arg <- relArgs
        if m.arguments.get(arg).nonEmpty
      } yield arg //name of args actually present in the mention

      //this should create args with time attachments
      newArgs = for {
        oldArg <- oldArgs //argName
        oldArgMention = m.arguments(oldArg).head
        time: Option[TimEx] = if (times.isDefined) times.get(m.sentence).find(_.span.end == oldArgMention.endOffset) else None //todo:need to check for an overlap and not start/end; same for loc
        if time.nonEmpty
        newArg = oldArgMention.withAttachment(new Time(time.head))

      } yield Map(oldArg -> Seq(newArg))

      updatedArgs = m.arguments ++ newArgs.flatten.toMap //old arguments ++ the newly created args with attachments.

    } yield copyWithNewArgs(m, updatedArgs) //create a copy of the original event mention, but with the arguments that also contain attachments
    handled
  }

  //todo: move to mention utils (eventually) or possible remove altogether
  def attachGeoLoc(mentions: Seq[Mention]): Seq[Mention] = {
    val relArgs = Array("moveTo", "moveFrom", "moveThrough") //location-related args in migration events

    val handled = for {
      m <- mentions
      geolocs = m.document.asInstanceOf[EidosDocument].geolocs
      oldArgs = for {
        arg <- relArgs
        if m.arguments.get(arg).nonEmpty
      } yield arg //name of args actually present in the mention

      //this should create args with geoloc attachments
      newArgs = for {
        oldArg <- oldArgs
        oldArgMention = m.arguments(oldArg).head
        location: Option[GeoPhraseID] = if (geolocs.isDefined) geolocs.get(m.sentence).find(_.startOffset == oldArgMention.startOffset) else None
        if location.nonEmpty
        newArg = oldArgMention.withAttachment(new Location(location.head))

      } yield Map(oldArg -> Seq(newArg))

      updatedArgs = m.arguments ++ newArgs.flatten.toMap //old arguments ++ the newly created args with attachments.

    } yield copyWithNewArgs(m, updatedArgs) //create a copy of the original event mention, but with the arguments that
    //also contain attachments
    handled
  }

  def assembleFragments(mentions: Seq[Mention]): Seq[Mention] = {
    // combine events with shared arguments AND combine events in close proximity with complementary arguments

    var orderedMentions = orderMentions(mentions)

    // the events we will ultimately return
    var returnedEvents = Array[Mention]()

    // keep merging events until we have nothing acceptable left to merge
    var stillMerging = true

    //have to define the number of allowable loops bc currently there's no way to continue to the second loop
    var maxNumOfLoops = 2
    var numOfLoops = 0

    // loop and merge compatible events, add to mergedEvents
    while (numOfLoops <= maxNumOfLoops) {

      println("\nLOOPING")

      // the events we will ultimately return
      returnedEvents = Array[Mention]()

      // to keep track of what events we've merged
      var used = Array.fill(orderedMentions.length)(false)


      for (i <- orderedMentions.indices) {
        println("i-th mention-->" + i + " " + orderedMentions(i).text)
        stillMerging = false
//        println("still merging line 91: " + stillMerging.toString())
        // only merge events if the first of the pair hasn't already been merged (heuristic!)
        if (!used(i)) {
          for (j <- i+1 until orderedMentions.length) {
            println("j-th mention--->" + j + " " + orderedMentions(j).text)
            if (
            // the two events are within one sentence of each other
              (Math.abs(orderedMentions(i).sentence - orderedMentions(j).sentence) < 2
                // AND if both events have complementary arguments (no overlap)
                && orderedMentions(i).arguments.keys.toList.intersect(orderedMentions(j).arguments.keys.toList).isEmpty)
                // OR
                ||
                // if both events share an argument
                (orderedMentions(i).arguments.values.toList.intersect(orderedMentions(j).arguments.values.toList).nonEmpty
                  // AND other arguments don't overlap (size of value intersection != size of key intersection) //todo: @zupon, there are cases, where these numbers happen to be the same. Any way to modify this? Do we have to have this?
//                  && orderedMentions(i).arguments.keys.toList.intersect(orderedMentions(j).arguments.keys.toList).size != orderedMentions(i).arguments.values.toList.intersect(orderedMentions(j).arguments.values.toList).size
                  //AND NOT both args with overlapping argName are specific (i.e., don't merge if both mentions have some key information with the same argName---merging will delete one of them)
                  && !bothSpecific(orderedMentions(i), orderedMentions(j))
                  )

                ||
                //if within one sent of each other
                (Math.abs(orderedMentions(i).sentence - orderedMentions(j).sentence) < 2

              //AND events share the type of argument
                 && (orderedMentions(i).arguments.keys.toList.intersect(orderedMentions(j).arguments.keys.toList).nonEmpty

              //AND the argument of the overlapping type in the j-th mention is less specific (i.e., it's the type of mention that is supposed to take an attachment but does not have one)
                  //todo: does not work if the label is "Location-Expand"
                  //todo: do this for time, too
                  && (orderedMentions(j).arguments.exists(arg => arg._2.exists(tbh => (tbh.label matches "Location") && tbh.attachments.isEmpty))))
                  //AND the arg of the overlapping type in the i-th mention is specific
                  && (orderedMentions(i).arguments.exists(arg => arg._2.exists(tbh => (tbh.label matches "Location") && tbh.attachments.nonEmpty))))





            ) {

              // merge the two events into one new event, keeping arguments from both but if there's overlap, choose the more specific one
              val newArgs = mergeArgs(orderedMentions(i), orderedMentions(j))
//              val copy = copyWithNewArgs(orderedMentions(i), orderedMentions(j).arguments ++ orderedMentions(i).arguments)
              println("ORIG MENTION: " + orderedMentions(i).text)
              val copy = copyWithNewArgs(orderedMentions(i), newArgs)

              stillMerging = true
//              println("still merging line 124: " + stillMerging.toString())
              // return the new event if it isn't identical to an existing event
              if (!(returnedEvents contains copy)) {
                returnedEvents = returnedEvents :+ copy
                println("merged: " + orderedMentions(i).text + " AND " + orderedMentions(j).text + "\n" + "Resuling event: " + copy.text + "|")
              }
              used = used.updated(i,true)
              used = used.updated(j,true)
            }
          }
        }

      }



      // add unmerged events ('false' in used list)
      for (i <- orderedMentions.indices) {
        if (!used(i)) {
          returnedEvents = returnedEvents :+ orderedMentions(i)
        }
      }
      orderedMentions = returnedEvents
      for (e <- returnedEvents) println("returned after loop: " + e.text)
      println("end of loop")
      numOfLoops = numOfLoops + 1


    }

    returnedEvents

  }

  /*
  checks if both of the overlapping args are specific (AND are not the same arg because if they are the same argument, their...`specificity status` will be the same)
   */
  def bothSpecific(m1: Mention, m2: Mention): Boolean = {
    val overlappingArgNames = m1.arguments.keys.toList.intersect(m2.arguments.keys.toList)
    for (argName <- overlappingArgNames) {
      val relArg1 = m1.arguments(argName).head
      val relArg2 = m2.arguments(argName).head
      if ((relArg1.attachments.nonEmpty || (relArg1.text matches "\\d+.*")) && (relArg2.attachments.nonEmpty || (relArg2.text matches "\\d+.*")) && relArg1 != relArg2  ) return true
    }

    false
  }

  /*
  returns mentions in the order they appear in the document (based on sent index and tokenInterval of the mention)
   */
  //todo: may go to mention utils
  def orderMentions(mentions: Seq[Mention]): Seq[Mention] = {
    val grouped = mentions.groupBy(_.sentence)

    //contains all mentions ordered by sent and by order in the sent
    var mentionArray = ArrayBuffer[Mention]()

    //for every sentence (sorted)...
    for (i <- grouped.keys.toList.sorted) {
      //...sort the mentions and append them to the mention array
      val sorted = grouped(i).sortBy(_.tokenInterval)
      for (m <- sorted) mentionArray.append(m)
    }
    mentionArray
  }


  /*
  merges args of two mentions in such a way as to hopefully return the more specific arg in case of an overlap
   */
  //keep in migr utils

  def mergeArgs(mention1: Mention, mention2: Mention): Map[String, Seq[Mention]] = {
    var newArgs = scala.collection.mutable.Map[String, Seq[Mention]]()
    //overlapping argument names
    val overlappingArgs = mention1.arguments.keys.toList.intersect(mention2.arguments.keys.toList)
    //for every argument in the two mentions...
    for (arg <- mention1.arguments ++ mention2.arguments) {
      //if the argumentName is present in both of the mentions...
      if (overlappingArgs.contains(arg._1)) {
        //choose the more specific argument by checking if one of them contains an attachment or contains numbers
        val arg1 = mention1.arguments(arg._1)
        if (arg1.exists(tbm => tbm.attachments.nonEmpty || (tbm.text matches "\\d+.*"))) {
          newArgs = newArgs ++ Map(arg._1 -> arg1)
        } else {
          newArgs = newArgs ++ Map(arg._1 -> mention2.arguments(arg._1))
        }

      } else {
        newArgs = newArgs ++ Map(arg)
      }
    }

    //for (arg <- newArgs) println("new arg: " + arg._1 + " " + arg._2)
    //return the new set of arguments
    newArgs.toMap
  }

  //todo: place elsewhere --> mention utils
  //todo: is it generalizeable enough?

  def copyWithNewArgs(orig: Mention, expandedArgs: Map[String, Seq[Mention]], foundByAffix: Option[String] = None, mkNewInterval: Boolean = true): Mention = {
    // Helper method to get a token interval for the new event mention with expanded args
    def getNewTokenInterval(intervals: Seq[Interval]): Interval = Interval(intervals.minBy(_.start).start, intervals.maxBy(_.end).end)

    val newTokenInterval = if (mkNewInterval) {
      // All involved token intervals, both for the original event and the expanded arguments
      val allIntervals = Seq(orig.tokenInterval) ++ expandedArgs.values.flatten.map(arg => arg.tokenInterval)
      // Find the largest span from these intervals
      getNewTokenInterval(allIntervals)
    }
    else orig.tokenInterval

    val paths = for {
      (argName, argPathsMap) <- orig.paths
      origPath = argPathsMap(orig.arguments(argName).head)
    } yield (argName, Map(expandedArgs(argName).head -> origPath))

    // Make the copy based on the type of the Mention
    val copyFoundBy = if (foundByAffix.nonEmpty) s"${orig.foundBy}_$foundByAffix" else orig.foundBy

    val newArgsAsList = for {
      seqMen <- expandedArgs.values
      men <- seqMen
    } yield men

    //create a mention to return as either another EventMention but with expanded args (the 'else' part) or a crossSentenceEventMention if the args of the Event are from different sentences
    val newMention = if (newArgsAsList.exists(_.sentence != orig.sentence) ) {
      //      orig.asInstanceOf[EventMention].copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = Map.empty)
      new CrossSentenceEventMention(labels = orig.labels, tokenInterval = newTokenInterval, trigger = orig.asInstanceOf[EventMention].trigger, arguments = expandedArgs, Map.empty, orig.sentence, orig.document, keep = true, foundBy = orig.foundBy + "++ crossSentActions", attachments = Set.empty)

    }else {

      orig match {
        case tb: TextBoundMention => throw new RuntimeException("Textbound mentions are incompatible with argument expansion")
        case rm: RelationMention => rm.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy)
        case em: EventMention => em.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = paths)
      }
    }

    newMention

  }

}

