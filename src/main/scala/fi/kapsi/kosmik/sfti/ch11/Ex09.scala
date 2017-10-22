package fi.kapsi.kosmik.sfti.ch11

/**
  * Define an object PathComponents with an unapply operation class that extracts
  * the directory path and file name from an java.nio.file.Path . For example, the
  * file /home/cay/readme.txt has directory path /home/cay and file name readme.txt .
  */
object Ex09 {

  object PathComponents {
    def unapply(path: java.nio.file.Path): Option[(String, String)] = {
      if (path.getNameCount == 0) None
      else Some((path.getParent.toString, path.getFileName.toString))
    }
  }

}
