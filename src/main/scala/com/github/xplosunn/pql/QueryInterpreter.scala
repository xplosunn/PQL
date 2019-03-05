package com.github.xplosunn.pql

import com.github.xplosunn.pql.PQL.Query
import com.github.xplosunn.pql.PQL.Query.{Many, MaybeOne, One}

trait QueryInterpreter[F[_], Collection[_]] {
  def run[R, T](query: Query[MaybeOne,T]): F[Option[T]]
  def runOne[R, T](query: Query[One, T]): F[T]
  def runMany[R, T](query: Query[Many, T]): F[Collection[T]]
}
