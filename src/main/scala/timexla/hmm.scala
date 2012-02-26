package timexla

import scala.collection.mutable.{ListBuffer,Map => MutableMap}

/**
 * BIOMap is simply a way of aggregating BIOTags in one object, counting incoming tags,
 * and initializing each key to an arbitrary type.
 */
case class BIOMap[A]() {
  val internal = MutableMap[A, Array[Double]]()
  def +=(key: A, tag: BIOTag.Value) {
    // plus one smoothing! (eww, gross.)
    if (!internal.contains(key)) internal(key) = Array.fill(BIOTag.values.size)(0.1d) 
    internal(key)(tag.id) += 1d
  }
}

/**
 * State is a linked list for backtracking through a Viterbi sequence.
 * 
 * It inherits from Ordered so that you can compare two states based on their underlying probabilities
 * toList recursively flattens a State (linked list) into a normal Scala List
 */
case class State(logprob: Double, tag: BIOTag.Value,
  previous: Option[State]) extends Ordered[State] {
  def compare(that: State) = this.logprob.compare(that.logprob)
  def toList: List[State] = {
    previous match {
      case Some(actual) => actual.toList :+ this
      case _ => List(this)
    }
  }
}
object State {
  def Empty = State(Double.NegativeInfinity, BIOTag.B, None)
}

/**
 * Features intends to be a collection of feature functions, weights for which are learned automatically.
 * Soon, soon.
 */
object Features {
  val gazetteer = List(
    "minute", "hour", "day", "week", "month", "season", "quarter", "year",
    "last", "earlier", "later", "before", "past", "coming", "ago", "next", "previous",
    "yesterday", "tomorrow", "current", "beginning",
    "first", "second", "third", "fourth", "fifth", "sixth", 
    "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec", 
    "mon", "tues", "wednes", "thurs", "fri", "satur", "sun",
    "moment")
  val gazetteer_regex = ("(?i)("+gazetteer.mkString("|")+")").r

  def digit_ratio(token: String) = digit_count(token).toDouble / token.length
  def digit_count(token: String) = """\d""".r.findAllIn(token).length
  def in_gazetteer(token: String) = { gazetteer_regex.findFirstIn(token) != None }
}

case class Hmm(documents: Seq[Document], lambda: Double) {
  /* For the most part, our feature lists are (discrete-token-property -> BIO.distribution) pairs.
     When the token property is continuous, we will do K-means clustering after the fact. Or not.  */ 

  val unigram_counts = MutableMap[String, Int]().withDefaultValue(1)
  val emissions = ListBuffer[(BIOTag.Value, String)]()

  val ratios = ListBuffer[(Double, BIOTag.Value)]()
  val digits = BIOMap[Int]()
  val gazette_matches = BIOMap[Boolean]()

  val transitions = ListBuffer[(BIOTag.Value, BIOTag.Value)]()
  val tags = ListBuffer[BIOTag.Value]()

  // e.g.: val tags = List(BIOTag.B, BIOTag.I, BIOTag.O, BIOTag.O, BIOTag.B, BIOTag.I)
  // println("Going through "+documents.length+" documents.")
  documents.foreach { document =>
    document.tokens.zip(document.tags).foreach { case (token, tag) =>
      // unigram_bio_counts += (token, tag)
      emissions += Tuple2(tag, token)
      ratios += Tuple2(Features.digit_ratio(token), tag)
      digits += (Features.digit_count(token), tag)
      gazette_matches += (Features.in_gazetteer(token), tag)
      unigram_counts(token) += 1
    }

    // l.zip(l.tail) == l.sliding(2) except it returns a list of tuples instead of a iterator of lists
    tags ++= document.tags
    transitions ++= document.tags.zip(document.tags.tail)
  }

  // ratios.groupBy(_._2) = List[tag: BIOTag, pair: ListBuffer[(ratio, tag)]]
  val tag_ratio_centers = ratios.groupBy(_._2).map { case (tag, pair) =>
    (tag, QuickMath.avg(pair.map(_._1)))
  }
  
  

  val tag_count = tags.length.toDouble
  val tag_probs = tags.groupBy(x => x).mapValues(_.length/tag_count)
  // transition_counts is a Map(tag0)(tag1) = count
  val transition_counts = transitions.groupBy(_._1).mapValues { tag_pair_list =>
    tag_pair_list.groupBy(_._2).mapValues(_.length).withDefaultValue(0)
  }.withDefaultValue(Map[BIOTag.Value, Int]())
  val transition_logprobs = BIOTag.values.map { tag0 =>
    val tag1_counts = transition_counts(tag0)
    // val tag1_counts = tag1_counts_unaltered.updated(BIOTag.B,
      // tag1_counts_unaltered(BIOTag.B) + tag1_counts_unaltered(BIOTag.I))
    val hapax_count = tag1_counts.count(_._2 == 1)
    val probs = BIOTag.values.map { tag1 =>
      tag1 -> ((hapax_count + lambda) * tag_probs(tag1) + tag1_counts(tag1))
    }.toMap
    val sum = probs.values.sum
    tag0 -> probs.mapValues(prob => math.log(prob/sum))
  }.toMap

  val smoothed_total = 1 + unigram_counts.values.sum.toDouble
  val unigram_probs = unigram_counts.mapValues(_/smoothed_total).toMap.withDefaultValue(1/smoothed_total)

  // emissions is List[tag: BIOTag, token: String]
  // emission_counts is Map(tag)(token) = count
  val emission_counts_raw = emissions.groupBy(_._1).mapValues { tag_token_list =>
    tag_token_list.groupBy(_._2).mapValues(_.length).withDefaultValue(0)
  }
  val emission_counts = emission_counts_raw.updated(BIOTag.B,
    emission_counts_raw(BIOTag.B).map { case (token, count) => 
      token -> (count + emission_counts_raw(BIOTag.I)(token))
    }
  )
  val emission_logprobs = emission_counts.map { case (tag, token_counts) =>
    // get the count of words that happened only once
    val hapax_count = token_counts.count(_._2 == 1)
    val smoothed_counts = token_counts.map { case (token, count) =>
      token -> (count + (lambda + hapax_count) * unigram_probs(token))
    }
    val sum = lambda + smoothed_counts.values.sum
    tag -> smoothed_counts.mapValues(count => math.log(count/sum)).withDefaultValue {
      math.log((lambda + hapax_count) * (1 / smoothed_total) / sum)
    }
  }

  def tag(unaligned_tokens: Seq[String]) = {
    val tokens = "###" +: unaligned_tokens :+ "###"
    // not really true, it's more like B: 0.5, I: 0, O: 0.5
    val alpha = Array.fill(tokens.length, BIOTag.values.size)(State.Empty)
    BIOTag.values.foreach { tag => alpha(0)(tag.id) = State(math.log(1d/3), tag, None) }
    (1 until (tokens.length - 1)).foreach { i =>
      // println("--"+tokens(i)+"--")
      BIOTag.values.foreach { tag =>
        // this will be extended with the other features
        val emission_logprob = emission_logprobs(tag)(tokens(i)) 

        /* for each of the previous states, take the probability of coming into this node from the previous
             node, times the probabilty of the previous node (and store the state you came from for each)
             thus, even if the transition is really likely, if the path to get there is unlikely,
             we won't take it  */
        val inbound = alpha(i - 1).map { previous_state =>
          val transition = transition_logprobs(previous_state.tag)(tag) // math.log(1d/6)
          (previous_state.logprob + transition, previous_state)
        }
        // println("->"+tag+"     "+inbound.map { x => 
        //   val color = if (x == inbound.max) Console.BLUE else ""
        //   "%s%s-> %3.4f%s" format (color, x._2.tag, x._1, Console.RESET)
        // }.mkString("   "))
        // println("  * emission_logprob (%3.4f) = %3.4f (%s->)" format
        //   (emission_logprob, inbound.max._1 + emission_logprob, inbound.max._2.tag))

        alpha(i)(tag.id) = State(inbound.max._1 + emission_logprob, tag, Some(inbound.max._2))
      }
    }

    val alpha_max_sequence = alpha(alpha.length - 2).max.toList.drop(1)
    alpha_max_sequence.map(_.tag)
  }
}
