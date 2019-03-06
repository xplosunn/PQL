package com.github.xplosunn.pql

import org.specs2.mutable
import PQL._
import com.github.xplosunn.pql.PQL.Query.{Many, MaybeOne, One, Results}
import org.specs2.matcher.MatchResult

trait QuerySpec extends mutable.Specification {

  def checkTypedQuery[R <: Results, T](expectedSql: String, query: Query[R, T]): MatchResult[String] = {
    query.sql must beEqualTo(expectedSql)
  }

}
