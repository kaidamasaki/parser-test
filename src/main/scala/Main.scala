package com.socrata.ice.importer

object Main {
  def main(args: Array[String]) {
    val spec = """[col9001,(col5),upper(trim(col4)+'--'+col3).replace(/BRA--VO/,"bravo"),col2+";"+col1,slice(col6,8),slice(col7,0,4),JSON.stringify({f:col9,f2:{g:col10},f3:{}}),lower(col11.replace(/M/,"h")),title(col12.replace(/out/i,"In").replace(/i/gi,"i")).replace(/I/,"i"),col13+lower(toStateCode(col14).replace(/x/i,"t"))]"""
    val row = List("lie","char",
                   "vo"," bra ",
                   "alfa",
                   "a small delta",
                   "echo echo echo!",
                   "ignored",
                   "foxtrot",
                   "golf",
                   "MOTEL",
                   "OUTdIa",
                   "julie",
                   "texas")

    val tree = ast.Transform(spec)
    println(tree)
    tree match {
      case Some(parsed) => println(parsed(row))
      case None => println("Failed to parse!")
    }
  }
}
