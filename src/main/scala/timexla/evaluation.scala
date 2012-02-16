package ut.timexla

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
        val gold_set = doc.spans.map(_.range).toSet 

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
