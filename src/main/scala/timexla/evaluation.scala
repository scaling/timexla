package timexla

import java.io.File
import scala.collection.mutable.{ListBuffer,Map => MutableMap}

case class PR() {
  var fp = 0
  var tp = 0
  var fn = 0
  
  def increment(fp_add: Int, tp_add: Int, fn_add: Int) {
    fp += fp_add
    tp += tp_add
    fn += fn_add
  }
  
  override def toString = {
    val precision = tp.toDouble / (tp + fp)
    val recall = tp.toDouble / (tp + fn)
    val fscore = 2 * ((precision * recall) / (precision + recall))

    """\hline\\
  Precision: & %1.3f \\
  Recall: & %1.3f \\
  F-measure: & %1.3f \\\hline
  """.format(precision, recall, fscore)
  }
  
  def report {
    println("False positives: " + fp)
    println("True positives:  " + tp)
    println("False negatives: " + fn)
  }
}

object Evaluation {
  def shuffle(source: Array[_ <: AnyRef]) {
    java.util.Collections.shuffle(java.util.Arrays.asList(source:_*)) // shuffles in-place
  }

  // run /Volumes/Zooey/Dropbox/ut/timex/corpora/timebank_1_2/data/timeml
  def main(args: Array[String]) = {
    var directory = new File(args(0))
    println("Reading directory: "+directory)
    var file_list = directory.listFiles.take(1)
    var documents = file_list.map { file =>
      val timeml_text = io.Source.fromFile(file).mkString
      Document(timeml_text, file.getName)
    }
    println(documents(0))
  }

  def evaluate(documents: List[Document]) {
    // documents.take(10).foreach(doc => tagAndPrintDoc(doc, hmm))
    
    // for (sentence <- test_doc.sentences) {
    //   println("sentence: "+sentence)
    //   printWithOutput(sentence, Array.fill(sentence.length)("-"), hmm.tag(sentence))
    // }
    
    
    val pr = PR()

    val training_count = (documents.length * 0.8d).toInt
    for (i <- 0 to 10) {
      val documents_array = documents.toArray
      shuffle(documents_array)
      val (training, test) = documents_array.splitAt(training_count)
      val hmm = Hmm(training.toList, 0.1)

      test.foreach { doc =>
        val gold_set = doc.timexes.values.map(_.range).toSet 

        val predicted_tags = hmm.tag(doc.tokens)
        val predicted_spans = ListBuffer[(Int, Int)]()
        var span_begin = -1 // -1 means we're not in a span
        predicted_tags.zipWithIndex.foreach { case (tag, i) =>
          if (tag == BIOTag.B) {
            if (span_begin > -1)
              predicted_spans += Tuple2(span_begin, i - 1)
            span_begin = i
          }
          else if (tag == BIOTag.O && span_begin > -1) {
            predicted_spans += Tuple2(span_begin, i - 1)
            span_begin = -1
          }
        }
        val predicted_set = predicted_spans.toSet
      
        pr.increment(
          (predicted_set -- gold_set).size, // fp
           (predicted_set & gold_set).size, // tp
          (gold_set -- predicted_set).size) // fn
      }
    }
    
    pr.report
    println()
    println(pr.toString)
  }

  def printWithOutput(tokens: Seq[String], tag_lists: List[(String, List[BIOTag.Value])]) {
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

    //val hmm = Hmm(documents, 0.1)
//    tagAndPrintDoc(documents(0), hmm)

    // println("Actual test_doc.length: "+test_doc.tokens.length)
    // println("test_doc.sentences: "+test_doc.sentences.length)
    
    // evaluate(documents)
}
