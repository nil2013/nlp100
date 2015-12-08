package me.nsmr.nlp100

import org.scalatest.{ GivenWhenThen, FeatureSpec }
import Section1._

class Section1Test extends FeatureSpec with GivenWhenThen {

  feature("""Q.00: 文字列"stressed"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．""") {
    scenario("適切に動作する") {
      assert(question00 == "desserts")
    }
  }
  feature("""Q.01: 「パタトクカシーー」という文字列の1,3,5,7文字目を取り出して連結した文字列を得よ．""") {
    scenario("適切に動作する") {
      assert(question01 == "パトカー")
    }
  }
  feature("""Q.02: 「パトカー」＋「タクシー」の文字を先頭から交互に連結して文字列「パタトクカシーー」を得よ．""") {
    scenario("適切に動作する") {
      assert(question02 == "パタトクカシーー")
    }
  }
  feature("""Q.03: "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."という文を単語に分解し，各単語の（アルファベットの）文字数を先頭から出現順に並べたリストを作成せよ．""") {
    scenario("適切に動作する") {
      assert(question03 == Seq(3,1,4,1,5,9,2,6,5,3,5,8,9,7,9))
    }
  }
  feature("""Q.04: "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."という文を単語に分解し，1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語は先頭の1文字，それ以外の単語は先頭に2文字を取り出し，取り出した文字列から単語の位置（先頭から何番目の単語か）への連想配列（辞書型もしくはマップ型）を作成せよ．""") {
    scenario("適切に動作する") {
      assert(question04 == Map(
          'H  -> 1 , 'He -> 2 , 'Li -> 3,
          'Be -> 4 , 'B  -> 5 , 'C  -> 6,
          'N  -> 7 , 'O  -> 8 , 'F  -> 9,
          'Ne -> 10, 'Na -> 11, 'Mi -> 12,
          'Al -> 13, 'Si -> 14, 'P  -> 15,
          'S  -> 16, 'Cl -> 17, 'Ar -> 18,
          'K  -> 19, 'Ca -> 20
          ))
    }
  }
  feature("""与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．""") {
    scenario("""Q.05-1: n-gramを作る関数を用い，"I am an NLPer"という文から単語bi-gramを得よ．""") {
      assert(question05_1("I am an NLPer") == Seq("I"))
    }
    scenario("""Q.05: n-gramを作る関数を用い，"I am an NLPer"という文から文字bi-gramを得よ．""") {
      assert(question05_2("I am an NLPer") == Seq('I'))
    }
  }
  feature("""Q.06: "paraparaparadise"と"paragraph"に含まれる文字bi-gramの集合を，それぞれ, XとYとして求め，XとYの和集合，積集合，差集合を求めよ．さらに，'se'というbi-gramがXおよびYに含まれるかどうかを調べよ．""") {
    val (x, y) = ("paraparaparadise","paragraph")
    // pa ar ra ap pa ar ra ap pa ar ra ad di is se
    // pa ar ra ag gr ra ap ph
    scenario("和集合を求める") {
      assert(question06_1(x, y) == Set(
          ('p', 'a'), ('a', 'r'), ('r', 'a'), ('a', 'g'), ('g', 'r'), ('r', 'a'), ('a', 'p'), ('p', 'h'),
          ('a', 'd'), ('d', 'i'), ('i', 's'), ('s', 'e')
          ))
    }
    scenario("積集合を求める") {
      assert(question06_2(x, y) == Set( ('p', 'a'),('a', 'r'),('r', 'a'),('a', 'p') ))
    }
    scenario("差集合を求める") {
      assert(question06_3(x, y) == Set( ('a', 'g'), ('g', 'r'),('p', 'h') ))
    }
    scenario("'se'というbi-gramがXおよびYに含まれるかどうか") {
      assert(question06_4(x, y, ('s', 'e')))
    }
  }
  feature("""Q.07: 引数x, y, zを受け取り「x時のyはz」という文字列を返す関数を実装せよ．さらに，x=12, y="気温", z=22.4として，実行結果を確認せよ．""") {
    scenario("適切に動作する") {
      assert(question07(12,"気温", 22.4)=="12時の気温は22.4")
    }
  }
  feature("""Q.08: 与えられた文字列の各文字を，以下の仕様で変換する関数cipherを実装せよ．
 * 英小文字ならば(219 - 文字コード)の文字に置換
 * その他の文字はそのまま出力
この関数を用い，英語のメッセージを暗号化・復号化せよ．""") {
    scenario("テスト") {
      assert(question08("I am a NLPer.") == "I zn z NLPvi.")
    }
  }
  feature("""Q.09: スペースで区切られた単語列に対して，各単語の先頭と末尾の文字は残し，それ以外の文字の順序をランダムに並び替えるプログラムを作成せよ．ただし，長さが４以下の単語は並び替えないこととする．適当な英語の文（例えば"I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."）を与え，その実行結果を確認せよ．""") {
    scenario(""" "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ." で実行してみる""") {
      println(question09("I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."))
    }
  }
}
