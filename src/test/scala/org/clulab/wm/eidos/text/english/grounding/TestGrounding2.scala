package org.clulab.wm.eidos.text.english.grounding

import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.Seq

class TestGrounding2 extends EnglishTest {
  // Grounding needs to be activated in englishTest.conf for these tests to be active.
  // They are usually not because of the large vector file that is required for realistic tests.
  // Furthermore, the appropriate grounder, such as wm_compositional needs to be activated.

  abstract class CompositionalGroundingTextTester {
    val groundTopN: Option[Int] = Option(5)
    val threshold: Option[Float] = Option(0.0f)
    val active: Boolean

    def fakeAnnotatedDoc(text: String, causeIntervals: Seq[Interval], effectIntervals: Seq[Interval],
        topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold):
        (Seq[EidosMention], Seq[EidosMention])

    def allGroundingNames(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Seq[String]
    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float], windowSize:Int): Seq[String]

  }

  object CompositionalGroundingTextTester {

    def apply(name: String): CompositionalGroundingTextTester = {
      val ontologyGrounderOpt: Option[OntologyGrounder] = ieSystem.components.ontologyHandler.ontologyGrounders.find { ontologyGrounder =>
        ontologyGrounder.name == name
      }

      ontologyGrounderOpt.map { ontologyGrounder =>
        new RealCompositionalGroundingTextTester(name, ontologyGrounder)
      }.getOrElse(new FakeCompositionalGroundingTextTester)
    }
  }

  class FakeCompositionalGroundingTextTester extends CompositionalGroundingTextTester {
    val active = false

    def fakeAnnotatedDoc(text: String, causeIntervals: Seq[Interval], effectIntervals: Seq[Interval],
        topN: Option[Int], threshold: Option[Float]):
        (Seq[EidosMention], Seq[EidosMention]) = (Seq.empty, Seq.empty)

    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[String] = Seq.empty
    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float], windowSize:Int): Seq[String] = Seq.empty

  }

  class RealCompositionalGroundingTextTester(name: String, ontologyGrounder: OntologyGrounder) extends CompositionalGroundingTextTester {
    val active = true

    def split(text: String): Array[String] = text.split(' ')

    def groundings(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): OntologyGroundings = {
      val ontologyGroundings: Seq[OntologyGrounding] = ontologyGrounder.groundOntology(mention, topN = groundTopN, threshold = threshold)
      val groundings = ontologyGroundings.map { ontologyGrounding =>
        val newName = name + ontologyGrounding.branch.map { branch => "/" + branch }.getOrElse("")

        newName -> ontologyGrounding
      }.toMap

      groundings
    }

    def groundings(mention: EidosMention, topN: Option[Int], threshold: Option[Float], windowSize:Int): OntologyGroundings = {
      val ontologyGroundings: Seq[OntologyGrounding] =
        if (ontologyGrounder.isInstanceOf[org.clulab.wm.eidos.groundings.CompositionalGrounder]){
          println("\tWe are using the correct grounding function!")
          ontologyGrounder.asInstanceOf[org.clulab.wm.eidos.groundings.CompositionalGrounder].groundOntology(mention, topN = groundTopN, threshold = threshold, windowSize = windowSize)
        }
        else{
          ontologyGrounder.groundOntology(mention, topN = groundTopN, threshold = threshold)
        }
      val groundings = ontologyGroundings.map { ontologyGrounding =>
        val newName = name + ontologyGrounding.branch.map { branch => "/" + branch }.getOrElse("")

        newName -> ontologyGrounding
      }.toMap

      groundings
    }

    protected def topGroundingValue(mention: EidosMention, componentName: String): Float = {
      val allGroundings = groundings(mention, groundTopN, threshold)
      val topGrounding = allGroundings(name + "/" + componentName).headOption.get._2
      topGrounding
    }

    // TODO Get these names from elsewhere
    def topConceptGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "concept")

    def topPropertyGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "property")

    def topProcessGrounding(mention: EidosMention): Float = topGroundingValue(mention: EidosMention, "process")

    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[String] = {
      val allGroundings = groundings(mention, topN, threshold)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => grounding._1.name }
      }.toSeq

      names
    }

    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float], windowSize:Int): Seq[String] = {
      val allGroundings = groundings(mention, topN, threshold, windowSize)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => grounding._1.name }
      }.toSeq


      val nameScoreTuple = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => (grounding._1.name, grounding._2 )}
      }.toSeq

      println("\tname score tuple:", nameScoreTuple)

      names

    }

    // to get both name AND score of groundings
    def allGroundingInfo(mention: EidosMention): Seq[(String,Float)] = {
      val allGroundings = groundings(mention, groundTopN, threshold)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => (grounding._1.name, grounding._2) }
      }.toSeq

      names
    }

    // TODO: pass topN and threshold through
    def fakeAnnotatedDoc(text: String, causeIntervals: Seq[Interval], effectIntervals: Seq[Interval],
                         topN: Option[Int], threshold: Option[Float]):
    (Seq[EidosMention], Seq[EidosMention]) = {
      val doc = ieSystem.annotate(text)
      val cause = {
        val odinCauses = causeIntervals.map(x =>
            new TextBoundMention(label = "Entity", x, 0, doc, true, "FakeRule"))
        val eidosCauses = EidosMention.asEidosMentions(odinCauses)

        ieSystem.components.ontologyHandler.ground(eidosCauses)
      }

      val effect = {
        val odinCauses = effectIntervals.map(x =>
            new TextBoundMention(label = "Entity", x, 0, doc, true, "FakeRule"))
        val eidosCauses = EidosMention.asEidosMentions(odinCauses)

        ieSystem.components.ontologyHandler.ground(eidosCauses)
      }

      val returned = (cause, effect)
      returned
    }
  }

  val tester: CompositionalGroundingTextTester = CompositionalGroundingTextTester("wm_compositional")

  {
    for (windowSize <- 0 to 4){

      behavior of "Grounding 1 Dup Window Size "+windowSize.toString

      val text = "He said it did not contradict African Union policy of no recognition for leaders who take power by force, since both men were taking measures to ensure free and transparent elections."

      val causeStart = 16
      val causeEnd = 17

      //val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(0, 4)), List(Interval(8, 10)))
      val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(causeStart, causeEnd)), Seq(Interval(causeStart, causeEnd)))

      val causeMentions = eidosMentions._1

      passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
        if (tester.active) {
          println("============================")
          println("window size:", windowSize)

          println("\t", causeMentions.head.odinMention.text)

          val matchedNodes = tester.allGroundingNames(causeMentions.head, tester.groundTopN, tester.threshold, windowSize)
          matchedNodes.contains(
            "concept/causal_factor/social_and_political/political/political_instability"
          ) should be(true)
        }
      }
    }
  }

  {
    for (windowSize <- 0 to 3){

      behavior of "Grounding 2 Dup Window Size "+windowSize.toString

      val text = "Edwards cited aid agencies inside Somalia as saying they remain concerned about landmines and other security threats which are making access extremely dangerous."

      val causeStart = 20
      val causeEnd = 21

      //val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(0, 4)), List(Interval(8, 10)))
      val eidosMentions = tester.fakeAnnotatedDoc(text, Seq(Interval(causeStart, causeEnd)), Seq(Interval(causeStart, causeEnd)))

      val causeMentions = eidosMentions._1

      passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
        if (tester.active) {
          println("============================")
          println("window size:", windowSize)
          println("\t", causeMentions.head.odinMention.text)

          val matchedNodes = tester.allGroundingNames(causeMentions.head, tester.groundTopN, tester.threshold, windowSize)
          matchedNodes.contains(
            "concept/causal_factor/infrastructure_access/road"
          ) should be(true)
        }
      }
    }
  }

}
