package com.github.xplosunn.pql

import com.github.xplosunn.pql.PQL.Query.One
import com.github.xplosunn.pql.PQL._

class DeleteSpec extends QuerySpec {

  case object SampleTable extends Table("table") {
    val col: Rep[String] = column(this, "col")
    val anotherCol: Rep[Int] = column(this, "another_col")
  }

  "Delete" >> {
    "all rows" >>
      checkTypedQuery[One, Int](
        "delete from table",
        from(SampleTable).delete
      )
    "where" >>
      checkTypedQuery[One, Int](
        "delete from table where table.another_col > 0",
        from(SampleTable).where(_.anotherCol > 0).delete
      )
  }
}
