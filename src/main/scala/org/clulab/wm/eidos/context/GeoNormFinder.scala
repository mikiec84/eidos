package org.clulab.wm.eidos.context

import java.io.File
import java.net.JarURLConnection
import java.net.URI
import java.net.URL
import java.nio.file.{Files, Path, Paths}
import java.util.IdentityHashMap

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.geonorm.{GeoLocationExtractor, GeoLocationNormalizer, GeoNamesIndex}
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StringUtils
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._

@SerialVersionUID(1L)
case class GeoPhraseID(text: String, geonameID: Option[Int], startOffset: Int, endOffset: Int)

object GeoNormFinder {

  private lazy val logger = LoggerFactory.getLogger(getClass)

  class CacheManager(config: Config) {
    val resourceSetting: String = config[String]("resource")
    val filesystemSetting: String = config[String]("filesystem")
    val sentinelSetting: String = config[String]("sentinel")

    val cacheFilesystemPath = Paths.get(filesystemSetting)
    val cacheSentinelPath = cacheFilesystemPath.resolve(sentinelSetting)
    val filesystemPath = cacheFilesystemPath.toAbsolutePath.normalize

    // The default is not to replace any files on a machine that is simply running Eidos.
    // This can be overruled by programs that are managing the cache.
    def mkCache(replaceOnUnzip: Boolean = false): Path = {
      val urlOpt = Option(this.getClass.getResource(resourceSetting))
      val url = urlOpt.getOrElse {
        throw new RuntimeException(s"ERROR: cannot locate the model resource $resourceSetting!")
      }
      val protocol = url.getProtocol

      if (protocol == "file") {
        val uri = new URI(url.toString)
        val alternateFilesystemPath = Paths.get(uri).toAbsolutePath.normalize

        alternateFilesystemPath
      }
      else if (protocol == "jar") {
        val jarUrl = url.openConnection().asInstanceOf[JarURLConnection].getJarFileURL
        val protocol2 = jarUrl.getProtocol
        assert(protocol2 == "file")
        val uri = new URI(jarUrl.toString)
        // This converts both percent encoded characters and file separators.
        val nativeJarFileName = new File(uri).getPath

        logger.info(s"Extracting the GeoNames index to $filesystemPath.")
        // Remove the leading slash and put on end because that's how they come out of the jar file.
        val prefix = resourceSetting.drop(1) + "/"
        FileUtils.unzip(Paths.get(nativeJarFileName), filesystemPath, prefix, replace = replaceOnUnzip)
        getCache.getOrElse {
          throw new RuntimeException(s"The caching operation was apparently unsuccessful.")
        }
      }
      else
        throw new RuntimeException(s"Unknown protocol for resource at $url.")
    }

    def getCache: Option[Path] = {
      val isFilesystemFile = Files.exists(cacheSentinelPath)

      if (isFilesystemFile) {
        logger.info(s"GeoNames index found at $filesystemPath.")
        Some(filesystemPath)
      }
      else {
        val urlOpt = Option(this.getClass.getResource(resourceSetting + "/" + sentinelSetting))
        val protocolOpt = urlOpt.map(_.getProtocol)
        val isResourceFile = protocolOpt.exists(_ == "file")

        if (isResourceFile) {
          val uri = new URI(this.getClass.getResource(resourceSetting).toString)
          val alternateFilesystemPath = Paths.get(uri).toAbsolutePath.normalize

          logger.info(s"GeoNames index found at $alternateFilesystemPath.")
          Some(alternateFilesystemPath)
        }
        else {
          logger.info(s"No GeoNames index as resource at $resourceSetting or as file at $filesystemPath.")
          None
        }
      }
    }
  }

  def fromConfig(config: Config): GeoNormFinder = {
    val cacheManager = new CacheManager(config)
    val oldCacheOpt = cacheManager.getCache
    val newCache = oldCacheOpt.getOrElse(cacheManager.mkCache())

    new GeoNormFinder(
      new GeoLocationExtractor(),
      new GeoLocationNormalizer(new GeoNamesIndex(newCache)))
  }

  def getGeoPhraseIDs(odinMentions: Seq[Mention]): Array[GeoPhraseID]= {
    val reachableMentions = EidosMention.findReachableMentions(odinMentions)
    val geoPhraseIDSeq: Seq[GeoPhraseID] = reachableMentions.flatMap { odinMention =>
      odinMention.attachments.collect {
        case attachment: Location => attachment.geoPhraseID
      }
    }
    val geoPhraseIDMap: IdentityHashMap[GeoPhraseID, Int] = geoPhraseIDSeq.foldLeft(new IdentityHashMap[GeoPhraseID, Int]()) { (identityHashMap, geoPhraseID) =>
      identityHashMap.put(geoPhraseID, 0)
      identityHashMap
    }
    val geoPhraseIDArray = geoPhraseIDMap
        .keySet
        .asScala
        .toArray
        .sortWith { (left: GeoPhraseID, right: GeoPhraseID) =>
          if (left.startOffset != right.startOffset)
            left.startOffset < right.startOffset
          else if (left.endOffset != right.endOffset)
            left.endOffset < right.endOffset
          else
            true
        }

    geoPhraseIDArray
  }

  def getGeoPhraseIDs(odinMentions: Seq[Mention], sentences: Array[Sentence]): Array[Seq[GeoPhraseID]]= {
    val geoPhraseIDs: Seq[GeoPhraseID] = getGeoPhraseIDs(odinMentions)
    val alignedGeoPhraseIDs: Array[Seq[GeoPhraseID]] = sentences.map { sentence =>
      val sentenceStart = sentence.startOffsets.head
      val sentenceEnd = sentence.endOffsets.last

      geoPhraseIDs.filter { geoPhraseID =>
        sentenceStart <= geoPhraseID.startOffset && geoPhraseID.endOffset <= sentenceEnd
      }
    }
    alignedGeoPhraseIDs
  }
}

class GeoNormFinder(extractor: GeoLocationExtractor, normalizer: GeoLocationNormalizer) extends Finder {

  def getGeoPhraseIDs(odinMentions: Seq[Mention], sentences: Array[Sentence]): Array[Seq[GeoPhraseID]] =
      GeoNormFinder.getGeoPhraseIDs(odinMentions, sentences)

  def find(doc: Document, initialState: State): Seq[Mention] = {
    val sentenceLocations = extractor(doc.sentences.map(_.raw))
    val Some(text) = doc.text
    val mentions = for {
      sentenceIndex <- doc.sentences.indices
      sentence = doc.sentences(sentenceIndex)
      locations = sentenceLocations(sentenceIndex)
      (wordStartIndex, wordEndIndex) <- locations
    } yield {
      val charStartIndex = sentence.startOffsets(wordStartIndex)
      val charEndIndex = sentence.endOffsets(wordEndIndex - 1)
      val locationPhrase = text.substring(charStartIndex, charEndIndex)
      val geoID = normalizer(text, (charStartIndex, charEndIndex)).headOption.map {
        case (entry, _) => entry.id.toInt
      }
      val geoPhraseID = GeoPhraseID(locationPhrase, geoID, charStartIndex, charEndIndex)

      new TextBoundMention(
        Seq("Location"),
        Interval(wordStartIndex, wordEndIndex),
        sentenceIndex,
        doc,
        true,
        getClass.getSimpleName,
        Set(Location(geoPhraseID))
      )
    }
    mentions
  }
}

