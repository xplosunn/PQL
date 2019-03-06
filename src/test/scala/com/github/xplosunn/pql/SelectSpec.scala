package com.github.xplosunn.pql

import com.github.xplosunn.pql.PQL.Query.{Many, MaybeOne}
import com.github.xplosunn.pql.PQL._

class SelectSpec extends QuerySpec {

  case object SampleTable extends Table("table") {
    val col: Rep[String] = column(this, "col")
    val anotherCol: Rep[Int] = column(this, "another_col")
  }

  case object SampleTable2 extends Table("table_two") {
    val col: Rep[String] = column(this, "col")
    val anotherCol: Rep[Int] = column(this, "another_col")
  }

  "Select" >> {
    "single column" >>
      checkTypedQuery[Many, String](
        "select table.col from table",
        from(SampleTable).select(_.col)
      )
    "two columns" >>
      checkTypedQuery[Many, (String, Int)](
        "select table.col, table.another_col from table",
        from(SampleTable).select2(t => (t.col, t.anotherCol))
      )

    "where" >>
      checkTypedQuery[Many, String](
        "select table.col from table where table.another_col > 3",
        from(SampleTable)
          .where(_.anotherCol > 3).
          select(_.col)
      )
    "where x and y" >>
      checkTypedQuery[Many, (String, Int)](
        "select table.col, table.another_col from table where table.another_col > 3 and table.another_col < 7",
        from(SampleTable)
          .where(_.anotherCol > 3)
          .and(_.anotherCol < 7)
          .select2(t => (t.col, t.anotherCol))
      )

    "join" >>
      checkTypedQuery[Many, (String, String)](
        "select table.col, table_two.col from table join table_two on table.another_col = table_two.another_col",
        from(SampleTable)
          .join(SampleTable2)
          .on((t1, t2) => equal(t1.anotherCol, t2.anotherCol))
          .select2 { case (t1, t2) => (t1.col, t2.col) }
      )
    "inner join" >>
      checkTypedQuery[Many, (String, String)](
        "select table.col, table_two.col from table inner join table_two on table.another_col = table_two.another_col",
        from(SampleTable)
          .innerJoin(SampleTable2)
          .on((t1, t2) => equal(t1.anotherCol, t2.anotherCol))
          .select2 { case (t1, t2) => (t1.col, t2.col) }
      )

    "limit 1" >>
      checkTypedQuery[MaybeOne, (String, String)](
        "select table.col, table.col from table limit 1",
        from(SampleTable)
          .select2 { t => (t.col, t.col) }
          .limit1
      )
    "limit X" >>
      checkTypedQuery[MaybeOne, (String, String)](
        "select table.col, table.col from table limit 10",
        from(SampleTable)
          .select2 { t => (t.col, t.col) }
          .limit(10)
      )
  }
}
