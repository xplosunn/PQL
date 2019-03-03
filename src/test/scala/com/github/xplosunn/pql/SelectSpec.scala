package com.github.xplosunn.pql

import org.specs2.mutable
import PQL._
import org.specs2.matcher.MatchResult

class SelectSpec extends mutable.Specification {

  case object SampleTable extends Table("table") {
    val col: Rep[String] = Rep("table.col")
    val anotherCol: Rep[Int] = Rep("table.another_col")
  }

  case object SampleTable2 extends Table("table_two") {
    val col: Rep[String] = Rep("table_two.col")
    val anotherCol: Rep[Int] = Rep("table_two.another_col")
  }

  "Select" >> {
    "single column" >>
      checkTypedQuery[String](
        "select table.col from table",
        from(SampleTable).select(_.col)
      )
    "two columns" >>
      checkTypedQuery[(String, Int)](
        "select table.col, table.another_col from table",
        from(SampleTable).select2(t => (t.col, t.anotherCol))
      )

    "where" >>
      checkTypedQuery[String](
        "select table.col from table where table.another_col > 3",
        from(SampleTable)
          .where(_.anotherCol > 3).
          select(_.col)
      )
    "where x and y" >>
      checkTypedQuery[(String, Int)](
        "select table.col, table.another_col from table where table.another_col > 3 and table.another_col < 7",
        from(SampleTable)
          .where(_.anotherCol > 3)
          .and(_.anotherCol < 7)
          .select2(t => (t.col, t.anotherCol))
      )

    "join" >>
      checkTypedQuery[(String, String)](
        "select table.col, table_two.col from table join table_two on table.another_col = table_two.another_col",
        from(SampleTable)
          .join(SampleTable2)
          .on((t1, t2) => equal(t1.anotherCol, t2.anotherCol))
          .select2 { case (t1, t2) => (t1.col, t2.col) }
      )
    "inner join" >>
      checkTypedQuery[(String, String)](
        "select table.col, table_two.col from table inner join table_two on table.another_col = table_two.another_col",
        from(SampleTable)
          .innerJoin(SampleTable2)
          .on((t1, t2) => equal(t1.anotherCol, t2.anotherCol))
          .select2 { case (t1, t2) => (t1.col, t2.col) }
      )
  }

  def checkTypedQuery[T](expectedSql: String, query: Query[T]): MatchResult[String] = {
    query.sql must beEqualTo(expectedSql)
  }
}
