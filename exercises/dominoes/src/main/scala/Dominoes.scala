object Dominoes {

  def chain(dominoes: List[(Int, Int)]): Option[List[(Int, Int)]] =
    rec(Nil, dominoes)

  def rec(tail: List[(Int, Int)], doms: List[(Int,Int)]): Option[List[(Int,Int)]] =
    if (doms.isEmpty)
      if (tail.isEmpty || tail.head._1 == tail.last._2) Some(tail) else None
    else {
      doms
        .flatMap(d => List(d, d.swap))
        .filter(d => tail.isEmpty || d._1 == tail.last._2)
        .flatMap(d => rec(tail :+ d, rm(doms, d)))
        .headOption
    }

  def rm(list: List[(Int, Int)], value: (Int, Int)): List[(Int, Int)] =
    list.span(d => d != value && d != value.swap) match { case (h, t) => h ++ t.drop(1) }
}