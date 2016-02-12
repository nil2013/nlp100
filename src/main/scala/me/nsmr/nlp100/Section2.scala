package me.nsmr.nlp100

import java.io.File
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.PrintWriter

object Section2 extends Section {
  def target = new File(resource, "hightemp.txt")
  def col1 = new File(out, "col1.txt")
  def col2 = new File(out, "col2.txt")
  def merged = new File(out, "merged.txt")

  def using[A, R <: { def close() }](r : R)(body : R => A) : A =
    try {
      body(r)
    } finally {
      r.close()
    }

  def open[A](file: File)(body: Iterator[String] => A): A = {
    using(new BufferedReader(new InputStreamReader(new FileInputStream(file)))) {
      br => body(Iterator.continually(br.readLine).takeWhile(_ != null))
    }
  }

  def writing[A](file: File)(body: PrintWriter => A): A = using(new PrintWriter(new BufferedWriter(new FileWriter(file)))) (body)

  def getColAt(line: String, col: Int): String = {
    if(col>1) getColAt(line.dropWhile(_!='\t').dropWhile(_=='\t'), col - 1)
    else line.dropWhile(_=='\t').takeWhile(_!='\t')
  }

  def question10 = open(target)(_.size)

  def question11 = open(target)(_.toList).map(_.replaceAll("\t", " ")).mkString(System.lineSeparator)

  def question12 = {
    open(target) {
      it =>
        writing(col1) { col1 =>
          writing(col2) { col2 =>
            it.foreach { line =>
              col1.println(getColAt(line, 1))
              col2.println(getColAt(line, 2))
            }
          }
        }
    }
  }

  def question13 = {
    writing(merged) {
      merged => open(col1)(_.toList).zip(open(col2)(_.toList)).map { case (col1, col2) => col1 + "\t" + col2 }.foreach(merged.println(_))
    }
  }

  def question14: List[String] = question14(scala.io.StdIn.readInt)

  def question14(size: Int): List[String] = open(target)(_.take(size).toList)

  def question15: List[String] = question15(scala.io.StdIn.readInt)

  def question15(size: Int): List[String] = open(target)(_.toArray).takeRight(size).toList

  def question16: List[List[String]] = question16(scala.io.StdIn.readInt)

  def question16(size: Int): List[List[String]] = {

    def splitBy(list: List[String], count: Int): List[List[String]] = {
      if(count > 0) list.take(list.size/count) :: splitBy(list.drop(list.size/count), count - 1)
      else Nil
    }

    splitBy(open(target)(_.toList), size)
  }

  def question17 = open(target) { _.map(_.takeWhile(_!='\t')).toList }.toSet

  def question18 = open(target) { _.map(line => (line, getColAt(line, 3).toDouble)).toList }.sortBy(- _._2).map(_._1)

  def question19 = {
    val lines = open(target)(_.toList).map(line => (line, getColAt(line, 1)))

    val dfmap = lines.map(_._2).groupBy(c => c).map { case (k, v) => (k, v.size) }

    lines.map { case (line, col1) => (line, dfmap(col1)) }.sortBy(-_._2).map(_._1)
  }
}