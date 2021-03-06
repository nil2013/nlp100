package me.nsmr.nlp100

import scala.annotation.tailrec

/**
 * 第1章: 準備運動
 */
object Section1 extends Section {

  /**
   * 00. 文字列の逆順
   * 文字列"stressed"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．
   */
  def question00(input: String): String = input.reverse

  /**
   * 01. 「パタトクカシーー」
   * 「パタトクカシーー」という文字列の1,3,5,7文字目を取り出して連結した文字列を得よ．
   */
  def question01(input: String, positions: Int*): String = positions.map(i => input(i-1)).mkString

  /**
   * 02. 「パトカー」＋「タクシー」＝「パタトクカシーー」
   * 「パトカー」＋「タクシー」の文字を先頭から交互に連結して文字列「パタトクカシーー」を得よ．
   */
  def question02(x: String, y: String): String = {
    @tailrec def solve(x: List[Char], y: List[Char], result: List[Char] = Nil): String = {
      (x, y) match {
        case (Nil, _) => result.reverse.mkString
        case (_, Nil) => result.reverse.mkString
        case (x :: tailX, y :: tailY) => solve(tailX, tailY, y :: x :: result)
      }
    }
    solve(x.toList, y.toList)
  }

  /**
   * 03. 円周率
   * "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."という文を単語に分解し，各単語の（アルファベットの）文字数を先頭から出現順に並べたリストを作成せよ．
   */
  def question03(input: String): Seq[Int] = getWordSeq(input).map(_.size)

  def getWordSeq(input: String): List[String] = {
    @tailrec def solve(chars: List[Char], word: String, result: List[String]): List[String] = {
      chars match {
        case Nil if word.size == 0 => result.reverse
        case Nil if word.size >  0 => solve(Nil, "", word :: result)
        case char :: tail if char != ''' && (char < 48 || char.isSpaceChar) =>
          solve(tail, "", if(word.size>0){word :: result} else {result} )
        case char :: tail => solve(tail, word + char, result)
      }
    }
    solve(input.toList, "", Nil)
  }

  /**
   * 04. 元素記号
   * "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."という文を単語に分解し，1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語は先頭の1文字，それ以外の単語は先頭に2文字を取り出し，取り出した文字列から単語の位置（先頭から何番目の単語か）への連想配列（辞書型もしくはマップ型）を作成せよ．
   */
  def question04(input: String, positions: Int*): Map[Symbol, Int] = {
    val words = getWordSeq(input)
    @tailrec def solve(words: List[String], pos: Int, map: Map[Symbol, Int]): Map[Symbol, Int] = {
      words match {
        case Nil => map
        case word :: tail if positions contains pos => solve(tail, pos+1, map + (Symbol(word.substring(0, 1)) -> pos) )
        case word :: tail => solve(tail, pos+1, map + (Symbol(word.substring(0, 2)) -> pos) )
      }
    }
    solve(words, 1, Map())
  }

  /**
   * 05. n-gram
   * 与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．
   */
  def getNgram[T](input: Seq[T], size: Int): List[List[T]] = {
    def solve(list: List[T], result: List[List[T]]): List[List[T]] = {
      list match {
        case Nil => result.reverse
        case _ :: tail => solve(tail, list.take(size) :: result)
      }
    }
    solve(input.toList, Nil)
  }

  /** 05-1. 与えられた文字列から単語bi-gramを得る */
  def question05_1(input: String): Seq[(String, Option[String])] = getNgram(getWordSeq(input), 2).collect {
    case x :: y :: _ => (x, Some(y))
    case x :: Nil => (x, None)
  }
  /** 05-2. 与えられた文字列から文字bi-gramを得る */
  def question05_2(input: String): Seq[(Char, Option[Char])] = getWordSeq(input).map(word => getNgram(word.toList, 2).collect{
    case x :: y :: _ => (x, Some(y))
    case x :: Nil => (x, None)
  }).reduce(_++_)

  /**
   * 06. 集合
   * "paraparaparadise"と"paragraph"に含まれる文字bi-gramの集合を，それぞれ, XとYとして求め，XとYの和集合，積集合，差集合を求めよ．さらに，'se'というbi-gramがXおよびYに含まれるかどうかを調べよ．
   */
  def getBiGramSet(input: String): Set[(Char, Option[Char])] = getNgram(input.toList, 2).collect{
    case x :: y :: _ => (x, Some(y))
    case x :: Nil => (x, None)
  }.toSet
  /** 06-1. 2つの文字列のbi-gramの集合の和集合を求める */
  def question06_1(x: String, y: String): Set[(Char, Option[Char])] = getBiGramSet(x) union getBiGramSet(y)
  /** 06-2. 2つの文字列のbi-gramの集合の積集合を求める */
  def question06_2(x: String, y: String): Set[(Char, Option[Char])] = getBiGramSet(x) intersect getBiGramSet(y)
  /** 06-3. 2つの文字列のbi-gramの集合の差集合を求める */
  def question06_3(x: String, y: String): Set[(Char, Option[Char])] = getBiGramSet(x) diff getBiGramSet(y)
  /** 06-4. 文字列のbi-gramの集合に、'se'というbi-gramが含まれるか、検査する */
  def question06_4(input: String, target: (Char, Option[Char])): Boolean = getBiGramSet(input) contains target

  /**
   * 07. テンプレートによる文生成
   * 引数x, y, zを受け取り「x時のyはz」という文字列を返す関数を実装せよ．さらに，x=12, y="気温", z=22.4として，実行結果を確認せよ．
   */
  def question07(template: String, x: Any, y: Any, z: Any): String = {
    @tailrec def solve(chars: List[Char], result: List[Any] = Nil): String = {
      chars match {
        case Nil => result.reverse.mkString
        case 'x'  :: tail => solve(tail, x :: result)
        case 'y'  :: tail => solve(tail, y :: result)
        case 'z'  :: tail => solve(tail, z :: result)
        case char :: tail => solve(tail, char :: result)
      }
    }
    solve(template.toList)
  }

  /**
   * 08. 暗号文
   * 与えられた文字列の各文字を，以下の仕様で変換する関数cipherを実装せよ．
   * 
   * 英小文字ならば(219 - 文字コード)の文字に置換
   * その他の文字はそのまま出力
   * この関数を用い，英語のメッセージを暗号化・復号化せよ．
   */
  def question08(input: String): String = input.map {
    case x if x.isLower => (219-x).toChar
    case x => x
  }

  /**
   * 09. Typoglycemia
   * スペースで区切られた単語列に対して，各単語の先頭と末尾の文字は残し，それ以外の文字の順序をランダムに並び替えるプログラムを作成せよ．ただし，長さが４以下の単語は並び替えないこととする．適当な英語の文（例えば"I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."）を与え，その実行結果を確認せよ．
   */
  def question09(input: String): String = {
    @tailrec def solve(words: List[String], result: List[String]): List[String] = {
      words match {
        case Nil => result.reverse
        case head :: tail if head.size > 4 => solve(tail, typoglycemia(head) :: result)
        case head :: tail if head.size <=4 => solve(tail, head :: result)
      }
    }
    solve(getWordSeq(input), Nil).mkString(" ")
  }
  def typoglycemia(input: String): String = {
    val rnd = new scala.util.Random
    @tailrec def solve(chars: List[Char], first: Char, last: Char, result: List[Char]): String = {
      chars match {
        case Nil => (first :: (last :: result).reverse).mkString
        case char :: tail =>
          if(rnd.nextBoolean()) solve(tail, first, last, char :: result)
          else solve(tail, first, last, result :+ char)
      }
    }
    if(input.size<3) input
    else solve(input.toList.drop(1).take(input.size-2), input(0), input.last, Nil)
  }
  @tailrec def isTypoglycemiaSets(x: List[String], y: List[String]): Boolean = {
    (x, y) match {
      case (Nil, Nil) => true
      case (x :: tailX, y :: tailY) if (x.size == y.size && x.head == y.head && x.last == y.last) =>
        def structure(str: String) = str.groupBy(x => x).map{ case (c, str) => c -> str.size }
        if(structure(x) == structure(y)) isTypoglycemiaSets(tailX, tailY)
        else false
      case _ => false
    }
  }
}