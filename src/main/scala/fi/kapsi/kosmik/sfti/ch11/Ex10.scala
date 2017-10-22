package fi.kapsi.kosmik.sfti.ch11

/**
  * Modify the PathComponents object of the preceding exercise to instead define an
  * unapplySeq operation that extracts all path segments. For example, for the file
  * /home/cay/readme.txt , you should produce a sequence of three segments: home ,
  * cay , and readme.txt .
  */
object Ex10 {

  object SeqPathComponents {
    def unapplySeq(path: java.nio.file.Path): Option[Seq[String]] = {
      if (path.getNameCount == 0) None
      else {
        import scala.collection.JavaConverters._
        val es = for (e <- path.asScala) yield e.toString
        Some(es.toSeq)
      }
    }
  }

}
