package com.github.xplosunn.pql


object PQL {

  sealed trait Query[T] {
    def sql: String
  }

  case class Select[T](select: String, from: String, where: Option[String], join: List[String] = List.empty) extends Query[T] {
    def sql: String = s"select $select from $from${where.fold("")(" where " + _)}${join.reduceOption(_ + " " + _).map(" " + _).getOrElse("")}"
  }

  abstract class Table(val name: String)

  case class Rep[T](name: String) extends AnyVal {
    def >(other: T)(implicit num: Numeric[T]): Rep[Boolean] = {
      Rep(name + " > " + other.toString)
    }

    def <(other: T)(implicit num: Numeric[T]): Rep[Boolean] = {
      Rep(name + " < " + other.toString)
    }
  }

  def from[T <: Table](table: T): From[T] = From(table, table.name)

  def from2[T1 <: Table, T2 <: Table](t1: T1, t2: T2): From[(T1, T2)] = From((t1, t2), t1.name + ", " + t2.name)

  case class On[A, B](on: A => B)

  case class From[Tables](tables: Tables, from: String, where: Option[String] = None, join: List[String] = Nil) {

    def select[V1](row: Tables => Rep[V1]): Query[V1] = {
      Select(row(tables).name, from, where, join)
    }

    def select2[V1, V2](row: Tables => (Rep[V1], Rep[V2])): Query[(V1, V2)] = {
      val (v1, v2) = row(tables)
      Select(v1.name + ", " + v2.name, from, where, join)
    }

    def where[C1](predicate: Tables => Rep[C1]): From[Tables] with WhereOps = {
      val pred = where.fold(predicate(tables).name)(_ + " and " + predicate(tables).name)
      new From(tables, from, Some(pred)) with WhereOps
    }

    trait WhereOps { self: From[Tables] =>

      def and[C1](predicate: Tables => Rep[C1]): From[Tables] with WhereOps = {
        self.where(predicate)
      }
    }

    def join[T2 <: Table](otherTable: T2): On[(Tables, T2) => Rep[Boolean], From[(Tables, T2)]] =
      join("join", otherTable)

    def innerJoin[T2 <: Table](otherTable: T2): On[(Tables, T2) => Rep[Boolean], From[(Tables, T2)]] =
      join("inner join", otherTable)

    def outerJoin[T2 <: Table](otherTable: T2): On[(Tables, T2) => Rep[Boolean], From[(Tables, T2)]] =
      join("outer join", otherTable)

    private def join[T2 <: Table](join: String, otherTable: T2): On[(Tables, T2) => Rep[Boolean], From[(Tables, T2)]] = {
      val on: ((Tables, T2) => Rep[Boolean]) => From[(Tables, T2)] = {
        cond =>
          From((tables, otherTable), from, None, List(s"$join ${otherTable.name} on ${cond(tables, otherTable).name}"))
      }
      On(on)
    }
  }

  def equal[T](a: Rep[T], b: Rep[T]): Rep[Boolean] =
    Rep(a.name + " = " + b.name)
}


