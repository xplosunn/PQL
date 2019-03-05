package com.github.xplosunn.pql

import com.github.xplosunn.pql.PQL.Query

trait QueryInterpreter[F[_]] {
  def run[T](query: Query[T]): F[T]
}
