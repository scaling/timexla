package timexla

import scala.xml._
import scala.collection.mutable.{ListBuffer,Map => MutableMap}

case class Timex(
  range: (Int, Int),
  id: String,
  timex_type: String, // DATE or DURATION
  value: String,
  temporal_function: Boolean,
  document_creation_time: Boolean
) {
  val links = ListBuffer[Link]()
}
case class Event(
  range: (Int, Int),
  id: String,
  class_name: String
)
case class Signal(
  range: (Int, Int),
  id: String
)

object LinkType extends Enumeration {
 type LinkType = Value
 val T, S, A = Value
}

class Link(
  xml_id: String,
  rel_type: String,
  link_type: LinkType.Value,
  signal: Option[Signal]
)

case class EventInstance(
  xml_id: String,
  event: Event,
  tense: String,
  aspect: String,
  polarity: String,
  pos: String,
  modality: String
) {
  val links = ListBuffer[Link]()
}

class TimeLink(
  xml_id: String,
  rel_type: String,
  link_type: LinkType.Value,
  signal: Option[Signal],
  timex: Timex 
) extends Link(xml_id, rel_type, link_type, signal)

class EventInstanceLink(
  xml_id: String,
  rel_type: String,
  link_type: LinkType.Value,
  signal: Option[Signal],
  event_instance: EventInstance
) extends Link(xml_id, rel_type, link_type, signal)

class SubordinatedEventInstanceLink(
  xml_id: String,
  rel_type: String,
  link_type: LinkType.Value,
  signal: Option[Signal],
  subordinated_event_instance: EventInstance
) extends Link(xml_id, rel_type, link_type, signal)

case class Document(text: String, filename: String) {
  val tokens = ListBuffer[String]() // text.split(" ").toList
  val xml_document = XML.loadString(text)
  println("----------------------------------- \"\"\"")
  println(text)
  println("\"\"\" -----------------------------------")
  /* debug: 
    // cd /Volumes/Zooey/Dropbox/ut/timex/corpora/timebank_1_2/data/timeml
	import scala.xml._
	val text = io.Source.fromFile("wsj_0670.tml").mkString
	val xml_doc = XML.loadString(text)
   */
  
  def tokenize(str1: String): Seq[String] = {
    // prefix all punctuation with a space: `his dolls. And' -> `his dolls . And'
    // Also: prefix all contractions with a space
	// xxx: need special handling for dollar/decimal amounts?    
    val str2 = """'\w{1,3}|[;:,.!?]+ """.r.replaceAllIn(str1, " $0").trim
    if (str2.isEmpty)
      List[String]()
    else
      """\s+""".r.split(str2)
  }
  
//  val tokenizer = """(?:[#\$]?\d+(?:\.\d+)?%?|(?:[-\w]+(?:'[-\w])*)|\.{2,}|\.|;|,|:|'|"|\(|\))""".r
  def addText(fragment: String): (Int, Int) = {
    // returns the start and end indices of the added tokens. start <= end
    val insert_at = tokens.length
    val new_tokens = tokenize(fragment)
    tokens ++= new_tokens
    
    (insert_at, insert_at + new_tokens.length) // is this a off-by-1 error?
  }

  val timexes = MutableMap[String, Timex]()
  val events = MutableMap[String, Event]()
  val signals = MutableMap[String, Signal]()
  val event_instances = MutableMap[String, EventInstance]()

  xml_document.child.foreach { child =>
    println(child)
    child.label match {
      case "#PCDATA" =>
        addText(child.text)
      case "TIMEX3" =>
        val range = addText(child.text)
        val timex = Timex(range,
          (child \ "@tid").text,
          (child \ "@type").text,
          (child \ "@value").text,
          (child \ "@temporalFunction").text.toBoolean,
          (child \ "@functionInDocument").text == "CREATION_TIME")
        timexes((child \ "@tid").text) = timex
      case "EVENT" =>
        val range = addText(child.text)
        val id = (child \ "@eid").text
        events(id) = Event(range, id, (child \ "@class").text)
      case "SIGNAL" =>
        val range = addText(child.text)
        val id = (child \ "@sid").text
        signals(id) = Signal(range, id)
      case "MAKEINSTANCE" =>
        val event = events((child \ "@eventID").text)
        val id = (child \ "@eiid").text
        event_instances(id) = EventInstance(id, event,
          (child \ "@tense").text,
          (child \ "@aspect").text,
          (child \ "@polarity").text,
          (child \ "@pos").text,
          (child \ "@modality").text)
      case "TLINK"|"SLINK"|"ALINK" =>
        val lid = (child \ "@lid").text
        val rel_type = (child \ "@relType").text
        val signal_id = (child \ "@signalID").text
        val link_type = child.label match {
          case "TLINK" => LinkType.T
          case "SLINK" => LinkType.S
          case "ALINK" => LinkType.A
        }
        val related_to_time = (child \ "@relatedToTime")
        val related_to_event_instance = (child \ "@relatedToEventInstance")
        val subordinated_event_instance = (child \ "@subordinatedEventInstance")
        
        val signal = signals.get(signal_id)

        val link = if (related_to_time.length > 0) {
          new TimeLink(lid, rel_type, link_type, signal, timexes(related_to_time.text))
        } else if (related_to_event_instance.length > 0) {
          new EventInstanceLink(lid, rel_type, link_type, signal, event_instances(related_to_event_instance.text))
        } else if (subordinated_event_instance.length > 0) {
          new SubordinatedEventInstanceLink(lid, rel_type, link_type, signal, event_instances(subordinated_event_instance.text))
        } else {
          new Link(lid, rel_type, link_type, signal) // what would this mean?
        }

        val eiid = (child \ "@eventInstanceID").text
        val tid = (child \ "@timeID").text
        if (event_instances.contains(eiid)) {
          event_instances(eiid).links += link
        }
        else if (timexes.contains(tid)) {
          timexes(tid).links += link
        }
        else {
          println(filename+": No receptacle for "+child)
        }
      case _ => 
        println("Nothing doing -- not a valid match "+child)
    }
  }
  
  // coverage will end up being a set of indices
  val event_indices = (Set[Int]() /: timexes.values) { (accumulator, next) =>
    accumulator ++ (next.range._1 to next.range._2) 
  }

  // List[BIOTag] enumerations
  val tags = (0 to tokens.length).map { i =>
    if (event_indices.contains(i)) {
      if (i > 0 && event_indices.contains(i - 1))
        BIOTag.I
      else
        BIOTag.B
    }
    else
      BIOTag.O
  }.toList

  override def toString = {
    tokens.zip(tags).map { case (token, tag) =>
      val color = tag match {
        case BIOTag.B => Console.GREEN
        case BIOTag.I => Console.YELLOW
        case BIOTag.O => Console.RED
      }
      color + token
    }.mkString(" ") + Console.RESET
//      println("%16s ".format(tokens(i)) + color + tags.mkString(" ") + )
//    filename+"\n"+text+"\n"+created+spans.mkString("\n")+"\n"+zipped.mkString("\n")
  }
}


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

