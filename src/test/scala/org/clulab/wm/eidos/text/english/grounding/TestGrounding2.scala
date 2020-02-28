package org.clulab.wm.eidos.text.english.grounding

import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.OntologyGrounder
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.Seq

class TestGrounding2 extends EnglishTest {
  // Grounding needs to be activated in englishTest.conf for these tests to be active.
  // They are usually not because of the large vector file that is required for realistic tests.
  // Furthermore, the appropriate grounder, such as wm_compositional needs to be activated.

  abstract class CompositionalGroundingTextTester {
    val groundTopN: Option[Int] = Option(5)
    val threshold: Option[Float] = Option(0.6f)
    val active: Boolean

    def fakeAnnotatedDoc(text: String, causeIntervals: List[Interval], effectIntervals: List[Interval],
                         topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold):
    (List[EidosMention], List[EidosMention])

    def allGroundingNames(mention: EidosMention, topN: Option[Int] = groundTopN, threshold: Option[Float] = threshold): Seq[String]
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

    def fakeAnnotatedDoc(text: String, causeIntervals: List[Interval], effectIntervals: List[Interval],
                         topN: Option[Int], threshold: Option[Float]):
    (List[EidosMention], List[EidosMention]) = (List.empty, List.empty)

    def allGroundingNames(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[String] = Seq.empty
  }

  class RealCompositionalGroundingTextTester(name: String, ontologyGrounder: OntologyGrounder) extends CompositionalGroundingTextTester {
    println("we are in real tester")

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

    protected def topGroundingValue(mention: EidosMention, componentName: String): Float = {
      val allGroundings = groundings(mention)
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

      val A = 0.01
      info(A.toString)

      names
    }
    // to get both name AND score of groundings
    def allGroundingInfo(mention: EidosMention): Seq[(String,Float)] = {
      val allGroundings = groundings(mention)
      val names = allGroundings.values.flatMap { ontologyGrounding =>
        ontologyGrounding.grounding.map { grounding => (grounding._1.name, grounding._2) }
      }.toSeq

      names
    }

    // TODO: pass topN and threshold through
    def fakeAnnotatedDoc(text: String, causeIntervals: List[Interval], effectIntervals: List[Interval],
                         topN: Option[Int], threshold: Option[Float]):
    (List[EidosMention], List[EidosMention]) = {
      val doc = ieSystem.annotate(text)
      val causeTBM = causeIntervals.map( x =>
        new TextBoundMention(label="Entity", x, 0, doc, true, "FakeRule") )
      val causeAD = causeTBM.map( x => AnnotatedDocument(doc, Seq(x)) )
      val cause = causeAD.map( x => ieSystem.components.ontologyHandler.process(x).eidosMentions.head)

      val effectTBM = effectIntervals.map( x =>
        new TextBoundMention(label="Entity", x, 0, doc, true, "FakeRule") )
      val effectAD = effectTBM.map( x => AnnotatedDocument(doc, Seq(x)) )
      val effect = effectAD.map( x => ieSystem.components.ontologyHandler.process(x).eidosMentions.head)

      val returned = (cause, effect)
      returned
    }
  }

  val tester: CompositionalGroundingTextTester = CompositionalGroundingTextTester("wm_compositional")

  {
    println("============================")
    for (windowSize <- 0 to 2){

      behavior of "Grounding 1 Dup Window Size "+windowSize.toString

      val text = "Conflict and widespread insecurity impact the humanitarian situation negatively and hamper humanitarian organizations from carrying out their activities in the deep field."

      val causeStart = scala.math.max(0, 4-windowSize)
      val causeEnd = scala.math.min(21, 5+windowSize)

      //val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(0, 4)), List(Interval(8, 10)))
      val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(causeStart, causeEnd)), List(Interval(causeStart, causeEnd)))

      val causeMentions = eidosMentions._1

      passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
        if (tester.active) {
          println("\t", causeMentions.head.odinMention.text)
          println("\t", tester.allGroundingNames(causeMentions.head))
          tester.allGroundingNames(causeMentions.head).contains(
            "concept/causal_factor/entity/person_and_group/humanitarian_workers"
          ) should be(true)
        }
      }
    }
    println("============================")
    for (windowSize <- 0 to 2){

      behavior of "Grounding 2 Dup Window Size "+windowSize.toString

      val text = "Nam, an economist-turned-politician, died of complications from testicular cancer on May 18."

      val causeStart = scala.math.max(0, 7-windowSize)
      val causeEnd = scala.math.min(14, 8+windowSize)

      //val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(0, 4)), List(Interval(8, 10)))
      val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(causeStart, causeEnd)), List(Interval(causeStart, causeEnd)))

      val causeMentions = eidosMentions._1

      passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
        if (tester.active) {
          println("\t", causeMentions.head.odinMention.text)
          println("\t", tester.allGroundingNames(causeMentions.head))
          tester.allGroundingNames(causeMentions.head).contains(
            "concept/causal_factor/environmental/climate_change_mitigation"
          ) should be(true)
        }
      }
    }
    println("============================")
    for (windowSize <- 0 to 2){

      behavior of "Grounding 3 Dup Window Size "+windowSize.toString

      val text = "He said it did not contradict African Union policy of no recognition for leaders who take power by force, since both men were taking measures to ensure free and transparent elections."

      val causeStart = scala.math.max(0, 16-windowSize)
      val causeEnd = scala.math.min(31, 17+windowSize)

      //val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(0, 4)), List(Interval(8, 10)))
      val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(causeStart, causeEnd)), List(Interval(causeStart, causeEnd)))

      val causeMentions = eidosMentions._1

      passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
        if (tester.active) {
          println("\t", causeMentions.head.odinMention.text)
          println("\t", tester.allGroundingNames(causeMentions.head))
          tester.allGroundingNames(causeMentions.head).contains(
            "concept/causal_factor/entity/person_and_group/leadership"
          ) should be(true)
        }
      }
    }
    println("============================")
    for (windowSize <- 0 to 2){

      behavior of "Grounding 4 Dup Window Size "+windowSize.toString

      val text = "Edwards cited aid agencies inside Somalia as saying they remain concerned about landmines and other security threats which are making access extremely dangerous."

      val causeStart = scala.math.max(0, 20-windowSize)
      val causeEnd = scala.math.min(23, 21+windowSize)

      //val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(0, 4)), List(Interval(8, 10)))
      val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(causeStart, causeEnd)), List(Interval(causeStart, causeEnd)))

      val causeMentions = eidosMentions._1

      passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
        if (tester.active) {
          println("\t", causeMentions.head.odinMention.text)
          println("\t", tester.allGroundingNames(causeMentions.head))
          tester.allGroundingNames(causeMentions.head).contains(
            "concept/causal_factor/social_and_political/threat/physical_insecurity"
          ) should be(true)
        }
      }
    }





  }

  ///// template for compositional grounder tests
  ///// add test name, sentence text, and token intervals for cause and effect mentions
  ///// if you have multiple causes/effects, see "Grounding 6" test for how to include them

  //  {
  //    behavior of "test name"
  //
  //    val text = "Sentence goes here"
  //    val eidosMentions = tester.fakeAnnotatedDoc(text, List(Interval(0,1)), List(Interval(1,2)))
  //    val causeMentions = eidosMentions._1
  //    val effectMentions = eidosMentions._2
  //
  //    passingTest should "process \"" + text + "\" cause correctly" taggedAs Somebody in {
  //      if (active) {
  //        tester.allGroundingNames(causeMentions.head).contains(
  //          "wm_compositional/???"
  //        ) should be (true)
  //      }
  //    }
  //    passingTest should "process \"" + text + "\" effect correctly" taggedAs Somebody in {
  //      if (active) {
  //        tester.allGroundingNames(effectMentions.head).contains(
  //          "wm_compositional/???"
  //        ) should be (true)
  //      }
  //    }
  //  }

}

