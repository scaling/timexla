package ut.timexla.timeml

import scala.xml._


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

// object Document {
  // def parse(text: String, filename: String) {
    // twitter.Json.parse(text) match {
      // case json_document: Map[String, Any] => {
        // the get below can't be getOrElse, btw
        // json_document.get("documents") match {
          // case Some(document_list: List[Map[String, Any]]) =>
            // document_list.map { document => Document(document) }
          // case _ => {
            // println("documents in JSON is not a list.")
            // List[Document]()
          // }
        // }
      // }
      // case _ => {
        // println("No documents in JSON.")
        // List[Document]()
      // }
    // }
//   }
// }

  // document = {
  //     filename: filename_str.tml,
  //     text: token1 token2 token3 ...,
  //     tokens: [[0,10], [11,19], ...],
  //     created: str,
  //     spans: [ { text: str, begin: int, end: int }, ... ]
  // }

  // val filename = dict.get("filename") match {
    // case Some(x: String) => x
    // case _ => ""
  // }
  // val text = dict.get("text") match {
    // case Some(x: String) => x
    // case _ => ""
  // }
  // val sentences = text.split(" ?\\.+ ?").map(_.split(" ").toList)
  // sentences.foreach(x => println("sent: "+x))

  // val token_indices = dict.get("tokens") match {
  //   case Some(token_list: List[List[Int]]) => {
  //     token_list.map { tuple: List[Int] => (tuple(0), tuple(1)) }
  //   }
  //   case _ => List[(Int, Int)]()
  // }


  // val created = dict.get("created") match {
  //   case Some(x: String) => x
  //   case _ => ""
  // }
  // spans is the List[Span] of timex's in the document, by timex3 spec and begin/end indices,
  //   where those indices are indices of the items in the tokens List[String]
  // val spans = dict.get("spans") match {
  //   // case Some(span_list: List[Map[String, Any]]) => span_list.map { tuple => Span(tuple) }
  //   case Some(span_list: List[Map[String, Any]]) => span_list.map(Span)
  //   case _ => List[Span]()
  // }

  // List[BIOTag] enumerations
  // val tags = (0 to tokens.length).map { i =>
  //   // println(i, spans.take(2), (spans(1).begin <= i && i <= spans(1).end).toString)
  //   val overlap = spans.find { span => span.begin <= i && i <= span.end }
  //   // println(overlap.toString)
  //   overlap match {
  //     case Some(x: Span) => {
  //       if (i == x.begin)
  //         BIOTag.B
  //       else
  //         BIOTag.I
  //     }
  //     case _ => BIOTag.O
  //   }
  // }.toList

  // override def toString = {
  //   val zipped = tokens.zip(tags).map { x =>
  //     val (token, tag) = x
  //     tag+" "+token
  //   }
  //   filename+"\n"+text+"\n"+created+spans.mkString("\n")+"\n"+zipped.mkString("\n")
  // }

        // token_index = @tokens.length # index of the very next token we'll add to @tokens
        // @spans << Span.new(span.value, token_index..(token_index + tokens.length - 1))
        // @tokens << Span.new(token, index..(index + token.length - 1))
        // index += token.length + 1

case class Timex(
  id: String,
  type: String,
  value: String,
  temporalFunction: Bool,
  documentCreationTime: Bool,
  range: (Int, Int)
)
case class Event(
  id: String,
  classname: String
)

case class Document(text: String, filename: String) {
  val tokens = ListBuffer[String]() // text.split(" ").toList
  val xml_document = XML.loadString(text)

  val tokenizer = """(?:[#\$]?\d+(?:\.\d+)?%?|(?:[-\w]+(?:'[-\w])*)|\.{2,}|\.|;|,|:|'|"|\(|\))""".r
  def addText(fragment: String): (Int, Int) = {
    // returns the start and end indices of the added tokens. start <= end
    val insert_at = tokens.length
    val new_tokens = tokenizer.findAllIn(text)
    tokens += new_tokens
    (insert_at, insert_at + new_tokens.length) // is this a off-by-1 error?
  }

    // val document = Document(filename)
    
    // (xml_document \\ "TLINK").foreach { tlink =>
      // xml_document
    // }
    // val tokens = 
  timexes = ListBuffer[Timex]()
  events = ListBuffer[Event]()

  xml_document.child.foreach { child =>
    child.label.match {
      case "#PCDATA" =>
        addText(child.text)
      case "TIMEX3" =>
        val range = addText(child.text)
        val timex = Timex((child \ "@tid").text,
          (child \ "@type").text,
          (child \ "@value").text,
          (child \ "@temporalFunction").text.toBool,
          (child \ "@functionInDocument").text == CREATION_TIME)
      case "EVENT" =>
        val timex = Timex3((child \ "@eid").text, (child \ "@class").text)
      case "SIGNAL" =>
        
    // print(x+"\n-\n")
  }

}


#rb
xml_doc.root.find('TLINK').each do |tlink_node|
  timeID = tlink_node.attributes['relatedToTime']
  signalID = tlink_node.attributes['signalID']
  if timeID && signalID
    timeID_tlinks[timeID] = signalID_tlinks[signalID] =
      TLink.new(timeID, signalID, tlink_node.attributes['relType'])
  end
end

spans = xml_doc.root.children.map do |node|
  span = if node.text?
    # node_type == XML::Node::TEXT_NODE
    Span.new(node.to_s)
  elsif node.name == 'TIMEX3'
    # XML::Node::ELEMENT_NODE
    # <TIMEX3 tid="t82" type="DATE" value="1998-01-08"
    #   temporalFunction="false" functionInDocument="CREATION_TIME">

    timex = Timex.new(node.first.to_s, node.attributes['value'], node.attributes['type'])
    timex.tlink = timeID_tlinks[node.attributes['tid']]

    if node.attributes['functionInDocument'] == 'CREATION_TIME'
      @created = timex.value #DateTime.strptime(node.attributes['value'], "%Y-%m-%d")
    end

    timex
  elsif node.name == 
    signalID = node.attributes['sid']
    if signalID_tlinks[signalID]
      signalID_tlinks[signalID].text = node.first.to_s
    end
    Span.new(node.first.to_s)
  else
    Span.new(node.first.to_s)
  end
  span.text = span.text.gsub(/\s+/, " ").strip
  span
end

@spans = spans.select{ |span| !span.text.empty? }