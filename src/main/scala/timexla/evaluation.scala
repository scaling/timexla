package timexla

import java.io.File
import scala.collection.mutable.{ListBuffer,Map => MutableMap}

object Evaluation {
  // run /Volumes/Zooey/Dropbox/ut/timex/corpora/timebank_1_2/data/timeml
  def main(args: Array[String]): Unit = {
    var directory = new File(args(0))
    println("Reading directory: "+directory)
    var file_list = directory.listFiles //.take(50)
    var documents = file_list.map { file =>
      // read string of text from file
      val timeml_text = io.Source.fromFile(file).mkString
      // parse it and return resulting document
      Document(timeml_text, file.getName)
    }
    //documents.foreach { doc => println(doc.fullString) }
    val pr = evaluate(documents)
    println(pr.shortString)
    println(pr.toString)
  }

  def shuffle(source: Array[_ <: AnyRef]) {
    java.util.Collections.shuffle(java.util.Arrays.asList(source:_*)) // shuffles in-place
  }

  def evaluate(documents: Seq[Document]): PrecisionRecall = {
    // documents.take(10).foreach(doc => tagAndPrintDoc(doc, hmm))
    
    // for (sentence <- test_doc.sentences) {
    //   println("sentence: "+sentence)
    //   printWithOutput(sentence, Array.fill(sentence.length)("-"), hmm.tag(sentence))
    // }
    
    
    val documents_array = documents.toArray
    shuffle(documents_array)

    
    val pr = PrecisionRecall()

    val training_count = (documents.length * 0.8d).toInt

      
    val (training, test) = documents_array.splitAt(training_count)
    println("Training on " + training.length + ", testing on " + test.length)
    
    val hmm = Hmm(training, 0.1)

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
    
    pr
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

case class PrecisionRecall() {
  var fp = 0
  var tp = 0
  var fn = 0
  
  def increment(fp_add: Int, tp_add: Int, fn_add: Int) {
    fp += fp_add
    tp += tp_add
    fn += fn_add
  }
  
  def precision = tp.toDouble / (tp + fp)
  def recall = tp.toDouble / (tp + fn)
  def fscore = 2 * ((precision * recall) / (precision + recall))
  
  override def toString = {
    List("Precision: " + precision, "Recall:    " + recall, "F-score:   " + fscore).mkString("\n")
  }
  def latex = {
    """\hline\\
  Precision: & %1.3f \\
  Recall: & %1.3f \\
  F-measure: & %1.3f \\\hline
  """.format(precision, recall, fscore)
  }
  
  def shortString = {
    List("False positives: " + fp, "True positives:  " + tp, "False negatives: " + fn).mkString("\n")
  }
}


