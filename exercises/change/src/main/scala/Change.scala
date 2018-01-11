object Change {
  def findFewestCoins(sum: Int, coins: Seq[Int]): Option[Seq[Int]] = {
    val cc = coins.sortBy(- _)
    val cache = scala.collection.mutable.Map[Int, Option[Seq[Int]]](0 -> Some(Nil))

    def rec(s: Int): Option[Seq[Int]] =
      cache.getOrElseUpdate(s,
        cc.filter(_ <= s).flatMap(coin => rec(s - coin).map(_ :+ coin)).sortBy(_.size).headOption
      )
    
    rec(sum).map(_.sorted)
  }
}