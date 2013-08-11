// Copyright 2012 by Christopher Brown - MIT Licensed
package timexla

import scala.collection.mutable.{Map => MutableMap}

object Tense extends Enumeration {
 type Tense = Value
 val Past, Present, Future = Value
}

class Timex3 {
  val store = MutableMap[String, String]()
  var duration = false
  var special = Option[Tense.Value](null)
  //  var millenium = ""
  //  var century = ""
  //  var decade = ""
  //  var unityear = ""
  //  var quarter = ""
  //  var half = ""
  //  var season = ""
  //  var month = ""
  //  var day = ""
  //  var hour = ""
  //  var minute = ""
  //  var second = ""
  //  var week = ""
  //  var dow = ""
  //  var tod = ""
  //  var duration = false
  //  var special = ""
          
  // def is_quarter(self):
  //     """Quarters can have at most the year also specified"""
  //     if self.quarter is not None:

  def set(key: String, value: String) = {
    key match {
      case "year" =>
        store("millenium") = value(0).toString
        store("century") = value(1).toString
        store("decade") = value(2).toString
        store("unityear") = value(3).toString
      case "tod2" =>
        store("tod") = value
      case _ =>
        store(key) = value
    }
  }
  def get(key: String): String = {
    key match {
      case "year" => List(store("millenium"), store("century"), store("decade"), store("unityear")).mkString
      case "tod2" => store("tod")
      case _ => store(key)
    }
  }
  
  override def toString = {
    special match {
      case Some(special_value) => special_value.toString
      case _ => (if (duration) "P" else "") + store.map { case (k, v) => k + "=" + v }.mkString(",") 
    }
  }
}

              
object Timex3 {
  val duration_re ="""(?x)
      ^P(
          ((?P<year>[X\d]+)Y)?
          ((?P<month>[X\d]+)M)?
          ((?P<day>[X\d]+)D)?
          (T
              ((?P<hour>[X\d]+)H)?
              ((?P<minute>[X\d]+)M)?
              ((?P<second>[X\d]+)S)?
          )?
      |
      ((?P<week>[X\d]+)W)
      |
      ((?P<quarter>[X\d]+)Q)
      )$
    """.r
  
  val special_re = "^(PAST_REF|PRESENT_REF|FUTURE_REF)$".r
  
  val absolute_re = """(?x)
      ^(?P<millenium>[\dX])
      (
          (?P<century>[\dX])
          (
              (?P<decade>[\dX])
              (
                  (?P<unityear>[\dX])
                  (-(
                      Q(?P<quarter>[X\d])
                      |
                      H(?P<half>[X\d])
                      |
                      (?P<season>SU|SP|FA|WI)
                      |
                      W(?P<week>[X\d]{2})
                      (-
                          (?P<dow>MO|TU|WE|TH|FR|SA|SU|\d)
                          (T
                              (?P<tod2>MO|AF|EV|NI)
                          )?
                      )?
                      |
                      (?P<month>[X\d]{2})
                      (-
                          (?P<day>[X\d]{2})
                          (T(
                              (?P<tod>MO|AF|EV|NI)
                              |
                              (?P<hour>[X\d]{2})
                              (:
                                  (?P<minute>[X\d]{2})
                                  (:
                                      (?P<second>[X\d]{2})
                                  )?
                              )?
                          ))?
                      )?
                  ))?
              )?
          )?
      )?
      $""".r
  
  val dow_int_to_str = Map(1 -> "MO", 2 -> "TU", 3 -> "WE", 4 -> "TH", 5 -> "FR", 6 -> "SA", 7 -> "SU")
  val dow_str_to_int = dow_int_to_str.map(_.swap)
  

  def parse(text: String): Timex3 = {
    val timex = new Timex3
    duration_re.findFirstMatchIn(text) match {
      case Some(rematch) =>
        timex.duration = true
        rematch.groupNames.foreach { groupName =>
          timex.set(groupName, rematch.group(groupName))
        }
      case _ =>
    }
    special_re.findFirstIn(text) match {
      case Some(rematch) =>
        timex.special = rematch match {
          case "PAST_REF" => Some(Tense.Past)
          case "PRESENT_REF" => Some(Tense.Present)
          case "FUTURE_REF" => Some(Tense.Future)
        }
      case _ =>
    }
    absolute_re.findFirstMatchIn(text) match {
      case Some(rematch) =>
        rematch.groupNames.foreach { groupName =>
          timex.set(groupName, rematch.group(groupName))
        }
      case _ =>
    }
    timex
  } 
  
  val test_lines = """1901
  1947-02
  197
  1971
  1972
  1974
  1978
  1979
  198
  1980
  1980-05-26
  1983
  1984
  1984-10
  1984-Q3
  1985
  1987
  1988
  1988-04
  1988-07
  1988-08
  1988-08-10
  1988-08-12
  1988-09-30
  1988-10
  1988-10-26
  1988-10-31
  1988-11
  1988-11-01
  1988-11-02
  1988-12
  1988-12-31
  1988-Q1
  1988-Q2
  1988-Q3
  1988-Q4
  1988-QX
  1988-SP
  1989
  1989-01
  1989-01-31
  1989-02
  1989-03
  1989-03-31
  1989-04
  1989-05
  1989-06
  1989-06-30
  1989-07
  1989-07-01
  1989-07-02
  1989-07-31
  1989-08
  1989-08-09
  1989-08-12
  1989-08-21
  1989-08-26
  1989-08-31
  1989-09
  1989-09-14
  1989-09-26
  1989-09-29
  1989-09-30
  1989-10
  1989-10-01
  1989-10-09
  1989-10-12
  1989-10-13
  1989-10-19
  1989-10-20
  1989-10-23
  1989-10-24
  1989-10-25
  1989-10-26
  1989-10-26TMO
  1989-10-26TNI
  1989-10-27
  1989-10-27T24
  1989-10-27TEV
  1989-10-27TMO
  1989-10-29
  1989-10-30
  1989-10-31
  1989-11
  1989-11-01
  1989-11-01TAF
  1989-11-02
  1989-11-06
  1989-11-06T17
  1989-11-08
  1989-11-09
  1989-11-09T17:00
  1989-11-10
  1989-11-13
  1989-11-15
  1989-11-16
  1989-11-17
  1989-11-20
  1989-11-21
  1989-11-22
  1989-11-27
  1989-11-30
  1989-12
  1989-12-01
  1989-12-07
  1989-12-10
  1989-12-14
  1989-12-15
  1989-12-21
  1989-12-31
  1989-FA
  1989-H2
  1989-Q1
  1989-Q2
  1989-Q3
  1989-Q4
  1989-QX
  1989-SP
  1989-SU
  1989-W42
  1989-W43-WE
  1989-W44
  1989-WXX
  1989-WXX-WE
  199
  1990
  1990-01
  1990-01-01
  1990-01-02
  1990-01-03
  1990-02
  1990-02-01
  1990-03
  1990-06-01
  1990-06-30
  1990-08
  1990-08-02
  1990-08-06
  1990-08-07
  1990-08-09
  1990-08-10
  1990-08-12
  1990-08-13
  1990-08-14
  1990-08-15
  1990-08-15T13:37
  1990-08-16
  1990-08-16T20:41
  1990-08-30
  1990-09
  1990-12-31
  1990-H1
  1990-Q1
  1990-Q3
  1990-W32-WE
  1990-W33
  1990-XX-XX
  1991
  1991-02-11
  1991-02-16T12:00
  1991-02-16TNI
  1991-02-18
  1991-02-23
  1991-02-23TNI
  1991-02-24
  1991-02-25
  1991-11-27
  1991-12-01
  1991-12-02
  1991-12-03
  1991-W08-WE
  1991-W48
  1992
  1992-11-17
  1993
  1994
  1994-04-06
  1994-06
  1994-12-31
  1994-WI
  1995
  1995-05-22
  1995-11
  1996
  1996-03
  1996-03-26
  1997
  1997-01
  1997-02
  1997-05
  1997-06
  1997-08-06
  1997-10
  1997-12
  1997-FA
  1997-Q4
  1998
  1998-01
  1998-01-10
  1998-01-11
  1998-01-13
  1998-01-14
  1998-01-15
  1998-01-19TNI
  1998-01-20
  1998-01-21
  1998-01-26
  1998-02
  1998-02-05
  1998-02-06
  1998-02-06T22:19:00
  1998-02-06T22:29:00
  1998-02-06TMO
  1998-02-11T22:00
  1998-02-12
  1998-02-12T01:58:00
  1998-02-12T12:00
  1998-02-12T19:15
  1998-02-12TEV
  1998-02-13
  1998-02-13T14:26:00
  1998-02-13T14:35:00
  1998-02-13T15:44:00
  1998-02-16
  1998-02-19
  1998-02-19T08:02:00
  1998-02-20
  1998-02-22
  1998-02-22TAF
  1998-02-23
  1998-02-27
  1998-02-27T07:56:00
  1998-02-27T08:02:00
  1998-02-27T08:09:00
  1998-02-27T08:13:00
  1998-02-27T08:17:00
  1998-03-01
  1998-03-01T14:11:00
  1998-03-03
  1998-03-04
  1998-03-05
  1998-03-06
  1998-03-06T13:19:00
  1998-03-06TAF
  1998-03-08
  1998-03-08T06:26:00
  1998-03-12
  1998-03-22
  1998-03-22T14:57:00
  1998-03-23
  1998-03-30
  1998-03-31
  1998-04-02
  1998-04-02T22:52:00
  1998-04-18
  1998-04-18T06:07:00
  1998-04-24T21:49:00
  1998-04-25
  1998-05
  1998-05-01
  1998-05-01T09:13:00
  1998-05-04
  1998-06-26
  1998-06-26T07:07:00
  1998-08
  1998-08-06
  1998-09
  1998-12
  1998-Q4
  1998-SU
  1998-W02
  1998-W04
  1998-W07
  1998-W08
  1998-W11
  1998-W12
  1998-W17
  1998-W19
  1998-WXX
  1998-XX-XXTNI
  1999
  1999-10-15
  199X
  19XX
  2000
  2004-11-01
  2007-03-15
  2009-11-01
  2011-03-01
  Done
  FUTURE_REF
  P100Y
  P14M
  P180D
  P18M
  P1D
  P1E
  P1M
  P1Q
  P1W
  P1Y
  P20M
  P20Y
  P210Y
  P22M
  P27Y
  P2D
  P2L
  P2M
  P2Q
  P2W
  P2Y
  P2Y6M
  P30D
  P3D
  P3M
  P3Y
  P40Y
  P4D
  P4M
  P4Y
  P52W
  P5D
  P5M
  P5Y
  P60D
  P6D
  P6M
  P8D
  P8M
  P8Y
  P90D
  P9M
  PAST_REF
  PRESENT_REF
  PT10H
  PT1H
  PT1M30S
  PT2H
  PT3H
  PT4H
  PXC
  PXD
  PXE
  PXM
  PXQ
  PXW
  PXY
  XXXX-Q2
  XXXX-Q4
  XXXX-SU
  XXXX-WXX-1TNI
  XXXX-WXX-2TNI
  XXXX-WXX-4"""

  def test = {
    test_lines.split("\n").foreach { line =>
      line match {
        case absolute_re(_) => true
        case special_re(_) => true
        case duration_re(_) => true
        case _ =>
          println("Failed to parse: " + line) 
      }
    }
  }
}
