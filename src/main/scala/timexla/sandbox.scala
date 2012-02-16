package ut.timexla

import java.io.File
import scala.collection.mutable.{ListBuffer,Map => MutableMap}

// run /Users/chbrown/work/timex/corpora/timebank_1_2.json

object Sandbox {
  def shuffle(source: Array[_ <: AnyRef]) {
    java.util.Collections.shuffle(java.util.Arrays.asList(source:_*)) // shuffles in-place
  }

  def printWithOutput(tokens: List[String], tag_lists: List[(String, List[BIOTag.Value])]) {
    val template = "%16s ".format("Token") + tag_lists.map(_._1).mkString(" ")
    (0 until tokens.length).foreach { i =>
      val tags = tag_lists.map(_._2(i))
      val identical = tags.zip(tags.tail).forall(ab => ab._1 == ab._2)
      val color = if (identical) {
        if (tags(0) == BIOTag.O)
          Console.YELLOW
        else
          Console.GREEN
      } else {
        Console.RED
      }
      println("%16s ".format(tokens(i)) + color + tags.mkString(" ") + Console.RESET)
    }
  }

  def tagAndPrintDoc(test_doc: Document, hmm: Hmm) {
    printWithOutput(test_doc.tokens, List(
      ("Gold", test_doc.tags),
      ("HMM", hmm.tag(test_doc.tokens))
    ))
  }

  def main(args: Array[String]) = {
    var directory = new File(args(0))
    // val filename = args(0)
    // println("Loading file: "+filename)
    var documents = directory.listFiles.map { filename =>
      val timeml_text = io.Source.fromFile(filename).mkString
      document = timeml.Document.parse(timeml_text, filename)
    }

    // val documents = JsonParsing.parse(text)

    // val text = lines.mkString("\n")
    // val test_doc = 

    // println("Actual test_doc.length: "+test_doc.tokens.length)
    // println("test_doc.sentences: "+test_doc.sentences.length)
    
    // evaluate(documents)
    val hmm = Hmm(documents, 0.1)
    tagAndPrintDoc(documents(0), hmm)
  }

}
