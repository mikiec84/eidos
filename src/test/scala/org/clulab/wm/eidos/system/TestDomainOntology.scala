package org.clulab.wm.eidos.system

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.groundings.CompactDomainOntology.CompactDomainOntologyBuilder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.Timer

class TestDomainOntology extends Test {

  def hasDuplicates(name: String, domainOntology: DomainOntology): Boolean = {
    val pathSeq = 0.until(domainOntology.size).map { i => domainOntology.getNamer(i).name }
    val pathSet = pathSeq.toSet

//    println(s"""The domain ontology "${domainOntology.name}" node count: ${ontologyNodes.length}""")
//    ontologyNodes.foreach(println)

    if (pathSeq.size != pathSet.size) {
      val pathBag = pathSeq.foldLeft(Map[String, Int]())((map, path) => map + (path -> (map.getOrElse(path, 0) + 1)))
      val duplicatePaths = pathBag.toSeq.filter(_._2 > 1).map(_._1)

      println(s"""The domain ontology "$name" includes duplicate nodes:""")
      duplicatePaths.foreach(println)
      true
    }
    else
      false
  }

  val proc = new FastNLPProcessor()
  val filter = true


  def show1(ontology: DomainOntology): Unit = {
    0.until(ontology.size).foreach { i =>
      println(ontology.getNamer(i).name + " = " + ontology.getValues(i).mkString(", "))
    }
    println
  }

  def show3(newOntology: DomainOntology, newerOntology: DomainOntology, newestOntology: DomainOntology): Unit = {
    show1(newOntology)
    show1(newerOntology)
    show1(newestOntology)
  }

  behavior of "un ontology"
  it should "load and not have duplicates" in {
    val newOntology = Timer.time("Load UN without cache") {
      UNOntology("/org/clulab/wm/eidos/ontologies/un_ontology.yml", "", proc, filter, loadSerialized = false)
    }
    val newerOntology = Timer.time("Load UN with cache") {
      //UNOntology("/org/clulab/wm/eidos/ontologies/un_ontology.yml", "", proc, filter, loadSerialized = true)
      new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
    }
    val newestOntology = Timer.time("Load UN with cache") {
      UNOntology("", "./cache/un.serialized", proc, filter, loadSerialized = true)
    }

    show3(newOntology, newerOntology, newestOntology)
  }

//  behavior of "fao ontology"
//  it should "load and not have duplicates" in {
//    val newOntology = Timer.time("Load FAO without cache") {
//      FAOOntology("/org/clulab/wm/eidos/ontologies/fao_variable_ontology.yml", "", proc, filter, loadSerialized =false)
//    }
//    val newerOntology = Timer.time("Load FAO with cache") {
//      //FAOOntology("/org/clulab/wm/eidos/ontologies/fao_variable_ontology.yml", "", proc, filter, loadSerialized = true)
//      new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
//    }
//    val newestOntology = Timer.time("Load FAO with cache") {
//      FAOOntology("", "./cache/fao.serialized", proc, filter, loadSerialized = true)
//    }
//
//    show3(newOntology, newerOntology, newestOntology)
//  }

//  behavior of "wdi ontology"
//  it should "load and not have duplicates" in {
//    val newOntology = Timer.time("Load WDI without cache") {
//      WDIOntology("/org/clulab/wm/eidos/ontologies/wdi_ontology.yml", "", proc, filter, loadSerialized = false)
//    }
//    val newerOntology = Timer.time("Load WDI with cache") {
//      //WDIOntology("/org/clulab/wm/eidos/ontologies/wdi_ontology.yml", "", proc, filter, loadSerialized = true)
//      new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
//    }
//    val newestOntology = Timer.time("Load WDI with cache") {
//      WDIOntology("", "./cache/wdi.serialized", proc, filter, loadSerialized = true)
//    }
//
//    show3(newOntology, newerOntology, newestOntology)
//  }

  behavior of "mesh ontology"
  it should "load and not have duplicates" in {
//    val newOntology = Timer.time("Load MeSH without cache") {
//      MeshOntology("/org/clulab/wm/eidos/ontologies/mesh_ontology.yml", "", proc, filter, loadSerialized = false)
//    }
//    val newerOntology = Timer.time("Load MeSH with cache") {
//      //MeshOntology("/org/clulab/wm/eidos/ontologies/mesh_ontology.yml", "", proc, filter, loadSerialized = true)
//      new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
//    }
    val newestOntology = Timer.time("Load MeSH with cache") {
      MeshOntology("", "./cache/mesh.serialized", proc, filter, loadSerialized = true)
    }

    show1(newestOntology)
//    show3(newOntology, newerOntology, newestOntology)
  }
}
