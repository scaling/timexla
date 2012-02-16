package ut.timexla

case class Span(dict: Map[String, Any]) {
  val text = dict.get("text") match {
    case Some(x: String) => x
    case _ => ""
  }
  val begin = dict.get("begin") match {
    case Some(x: Int) => x
    case _ => -1
  }
  val end = dict.get("end") match {
    case Some(x: Int) => x
    case _ => -1
  }

  val range = (begin, end)

  override def toString = {
    text+" ["+begin+"-"+end+"]"
  }
}

case class Document(dict: Map[String, Any]) {
  // document = {
  //     filename: filename_str.tml,
  //     text: token1 token2 token3 ...,
  //     tokens: [[0,10], [11,19], ...],
  //     created: str,
  //     spans: [ { text: str, begin: int, end: int }, ... ]
  // }

  val filename = dict.get("filename") match {
    case Some(x: String) => x
    case _ => ""
  }
  val text = dict.get("text") match {
    case Some(x: String) => x
    case _ => ""
  }
  val tokens = text.split(" ").toList
  // println("text: "+text)
  val sentences = text.split(" ?\\.+ ?").map(_.split(" ").toList)
  // sentences.foreach(x => println("sent: "+x))

  val token_indices = dict.get("tokens") match {
    case Some(token_list: List[List[Int]]) => {
      token_list.map { tuple: List[Int] => (tuple(0), tuple(1)) }
    }
    case _ => List[(Int, Int)]()
  }


  val created = dict.get("created") match {
    case Some(x: String) => x
    case _ => ""
  }
  // spans is the List[Span] of timex's in the document, by timex3 spec and begin/end indices,
  //   where those indices are indices of the items in the tokens List[String]
  val spans = dict.get("spans") match {
    // case Some(span_list: List[Map[String, Any]]) => span_list.map { tuple => Span(tuple) }
    case Some(span_list: List[Map[String, Any]]) => span_list.map(Span)
    case _ => List[Span]()
  }

  // List[BIOTag] enumerations
  val tags = (0 to tokens.length).map { i =>
    // println(i, spans.take(2), (spans(1).begin <= i && i <= spans(1).end).toString)
    val overlap = spans.find { span => span.begin <= i && i <= span.end }
    // println(overlap.toString)
    overlap match {
      case Some(x: Span) => {
        if (i == x.begin)
          BIOTag.B
        else
          BIOTag.I
      }
      case _ => BIOTag.O
    }
  }.toList

  override def toString = {
    val zipped = tokens.zip(tags).map { x =>
      val (token, tag) = x
      tag+" "+token
    }
    filename+"\n"+text+"\n"+created+spans.mkString("\n")+"\n"+zipped.mkString("\n")
  }
}

// run /Users/chbrown/work/timex/code/timebank.json
object JsonParsing {
  def parse(text: String) = {
    twitter.Json.parse(text) match {
      case json_document: Map[String, Any] => {
        // the get below can't be getOrElse, btw
        json_document.get("documents") match {
          case Some(document_list: List[Map[String, Any]]) =>
            document_list.map { document => Document(document) }
          case _ => {
            println("documents in JSON is not a list.")
            List[Document]()
          }
        }
      }
      case _ => {
        println("No documents in JSON.")
        List[Document]()
      }
    }
  }
}
