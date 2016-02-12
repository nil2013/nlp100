package me.nsmr.nlp100

import java.io.File

abstract class Section {
  def notYet: Nothing = throw new NotImplementedError("まだその問題問いてません")
  def resource: File = new File("resource")
  def out = {
    new File("out") match {
      case o if o.exists() && o.isDirectory() => o
      case o if !o.exists() => o.mkdirs(); o
      case o => throw new UnsupportedOperationException(s"'${o.getPath}'はファイルです。")
    }
  }
}