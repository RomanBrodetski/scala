// The basic idea is to enumerate all the grouping possibilities and compute the total sum for each.
// the algorithm operates only on the group numbers, not individual books,
// so the performance is good even for a lot of books
// Here are all the sensible grouping possibilities for the input 7x1,6x2,2x3,5x4,5x5 (the numbers are the group sizes):
// 1, 3, 4, 4, 4, 4, 5
// 1, 2, 4, 4, 4, 5, 5
// 2, 3, 4, 4, 4, 4, 4
// 2, 2, 3, 4, 4, 5, 5
// 2, 3, 3, 4, 4, 4, 5
// 1, 3, 3, 4, 4, 5, 5
// 2, 3, 3, 3, 4, 5, 5
// 2, 2, 4, 4, 4, 4, 5
// 3, 3, 3, 3, 4, 4, 5
// 3, 3, 3, 4, 4, 4, 4
// 1, 4, 4, 4, 4, 4, 4
// 3, 3, 3, 3, 3, 5, 5.


object BookStore {
  val BookPrice = 8
  val PriceByGroupSize = Map(
    1 -> 100,
    2 -> 95,
    3 -> 90,
    4 -> 80,
    5 -> 75
  ).map {
    case (amount, mul) => amount -> amount * BookPrice * mul
  }

  def total(list: List[Int]) = list match {
    case Nil => 0
    case ls =>
      // we only care about the numbers of groups of the same books, i.e. 2,2,2,1 is the same as 1,1,1,2
      val books = ls.groupBy(id => id).map(_._2.size)
      val init = Seq.fill(books.max)(0)
      books.foldLeft(Seq(init))((acc, num) =>
        acc.flatMap(addToTheGroups(_, num)).distinct).map(_.map(PriceByGroupSize).sum).min.toDouble / 100
  }

  // this method takes the initial group sizes as the argument (i.e. 3,2,1 is "two groups: one of 3 and one of 2 books")
  // and the number of the new books to add
  // and returns all possible combinations of the new group sizes
  // i.e. there are 2 ways to add two books to the three groups of 3,2 and 1: the result is
  // [4,3,1],[3,3,2],[4,2,2].

    def addToTheGroups(groups: Seq[Int], select: Int): Set[Seq[Int]] =
      if (select == 0)
        Set(groups)
      else
        groups.foldLeft(Set.empty[Seq[Int]]) { (a, option) =>
          a ++ addToTheGroups(rm(groups, option), select - 1).map(ls => (ls :+ (option + 1)).sorted)
        }

  def rm(list: Seq[Int], value: Int): Seq[Int] =
    list.span(_ != value) match { case (h, t) => h ++ t.drop(1) }

}