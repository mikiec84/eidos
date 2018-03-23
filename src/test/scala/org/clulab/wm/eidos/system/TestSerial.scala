package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.test.TestUtils.{ieSystem, _}

import scala.collection.Seq

class TestSerial extends Test {

  // This text has been found to be problematic
  protected val texts = Seq("""
      |South Sudan's principal trading partner is Uganda (Table 2.5). Customs officials believe that 80 percent of all goods imported into South Sudan enter through the Nimule-Torit-Juba route (WFP 2015). Trade with Sudan, South Sudan's second most important trading partner, has decreased significantly since independence. Trade between the two countries now is mainly informal. Most food from Sudan is transported from El Obeid to Aweil. Before the separation of the two countries, northern Sudan was a key provider of both food and fuel for southern Sudan. However, in 2012 the border was closed and supply was cut off (World Bank 2014). Since 2013 some borders have been reopened, but trade has been slow to recover. Kenya's trade with South Sudan is mainly in nonfood items. Ethiopia also trades with South Sudan, and in opposition-controlled areas, trade is now almost entirely with Ethiopia.
      |Although about 70 percent of total land area is suitable for cultivation in South Sudan, only 4 percent of the land is currently used for agriculture (Diao et al. 2011, Figure 2.1). Thus, there is ample scope for expansion of area cultivated. Currently, most crop cultivation is done on small farms, producing little if any marketable surplus. According to estimates for 2013 from the Food and Agriculture Organization of the United Nations (FAO) and the World Food Programme (WFP), 10 percent of the population of South Sudan (about 1 million people) are severely food insecure (FAO and WFP 2013).
      |Rainfall within the country ranges from 500 mm to 2,000 mm per year, with the highest levels of rainfall in the southern part of the country. Much of north central and northeastern South Sudan is located in flood plains along the Nile River (the Sudd) that are flooded during the rainy season but receive little rainfall otherwise (600 to 800 mm per year). The highest potential areas for agricultural production are Western Equatoria and the southern half of Central Equatoria, or the so-called Green Belt, where annual rainfall ranges from 900 to 2,000 mm per year (Table 2.6). Rainfall in the Hills and Mountains region of the northern half of Central Equatoria and the western half of Eastern Equatoria (500 to 800 mm per year) is also sufficient to support substantial crop agriculture (WFP 2011).
      |In most areas of South Sudan, soils are low in phosphorous as well as in organic matter. Despite this, most soils are moderately fertile. But soils could quickly become nutrient deficient due to leaching related to heavy rainfall, low levels of fertilizer use (on average only 4 kg per ha of fertilizers are used in South Sudan), and over-farming. Nutrient mining from maize production ranges between 30 and 60 kg per ha per year. Smallholders have limited knowledge and capacity, lack an understanding of the role of fertilizer in improved crop production, and do not have access to extension or transfer services--all factors that could lead to further soil nutrient depletion (Kowr 2013).
      |Institutional weakness has further hindered development of the agriculture sector. Disagreements over land rights for crop cultivation and livestock grazing continue to be a major source of conflict. This insecurity discourages farmers from expanding production, and traders and retailers from building marketing infrastructure. The extremely low level of public investment in recent decades has meant that essentially no irrigation infrastructure exists and only 2 percent of South Sudan's limited road network is paved. Roads are poorly maintained, not repaired, and completely washed out during the rainy season (World Bank 2012; USAID and Fintrac 2012). Because of this inadequate transportation infrastructure, it is difficult and expensive for subsistence farmers to transport surpluses to markets.
      |Further, poor business practices and a lack of information about market prices make it difficult for businesses to develop along the agriculture value chain. South Sudan receives a ranking of 186 out of 189 on ease of doing business in the World Bank 2015 Doing Business report (World Bank 2014). Furthermore, South Sudan receives a percentile rank of 5.7 in government effectiveness and of 3.0 in control of corruption in the World Bank's governance indicators (World Bank 2015).
      """.stripMargin
  )

  protected def toText(eidosSystem: EidosSystem): String = {
    val annotatedDocuments = texts.map(eidosSystem.extractFromText(_))
    val corpus = new JLDCorpus(annotatedDocuments, ieSystem)

    corpus.toJsonStr()
  }

  {
    val expected = toText(ieSystem)

    behavior of "serial EidosSystem calling of annotate"

    ignore should "be consistent" in {
      for (i <- 1 to 100) {
        println("Trial " + i)
        val actual = toText(ieSystem)

        if (expected != actual) {
          println("----- Expected -----")
          println(expected)
          println("----- Actual -----")
          println(actual)
        }
        expected should be(actual)
      }
    }
  }

  {
    def toNewText() = toText(new EidosSystem())

    val expected = toNewText()

    behavior of "serial new EidosSystem calling of annotate"

    it should "be consistent" in {
      for (i <- 1 to 100) {
        println("Trial " + i)
        val actual = toNewText()

        if (expected != actual) {
          println("----- Expected -----")
          println(expected)
          println("----- Actual -----")
          println(actual)
        }
        expected should be(actual)
      }
    }
  }
}
