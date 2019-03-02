package com.github.xplosunn.pql


object PQL {

  sealed trait Query[T] {
    def sql: String
  }

  case class Select[T](from: String, where: Option[String], select: String) extends Query[T] {
    def sql: String = s"select $select from $from${where.fold("")(" where " + _)}"
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

  def from[T <: Table](table: T): From[T] = From(table)

  def from2[T1 <: Table, T2 <: Table](t1: T1, t2: T2): From2[T1, T2] = From2((t1, t2))

  case class From[T <: Table](table: T, where: Option[String] = None) {
    def select[V1](row: T => Rep[V1]): Query[V1] = {
      Select(table.name, where, row(table).name)
    }

    def select2[V1, V2](row: T => (Rep[V1], Rep[V2])): Query[(V1, V2)] = {
      val (v1, v2) = row(table)
      Select(table.name, where, v1.name + ", " + v2.name)
    }

    def where[C1](predicate: T => Rep[C1]): From[T] with WhereOps = {
      val pred = where.fold(predicate(table).name)(_ + " and " + predicate(table).name)
      new From(table, Some(pred)) with WhereOps
    }

    def join[T2 <: Table](otherTable: T2): On[(T, T2) => Rep[Boolean], From2[T, T2]] = {
      val on: ((T, T2) => Rep[Boolean]) => From2[T, T2] = {
        cond =>
          From2((table, otherTable), Some(cond(table, otherTable).name))
      }
      On(on)
    }

    trait WhereOps { self: From[T] =>

      def and[C1](predicate: T => Rep[C1]): From[T] = {
        self.where(predicate)
      }
    }
  }

  case class On[A, B](on: A => B)

  case class From2[T1 <: Table, T2 <: Table](tables: (T1, T2), where: Option[String] = None) {
    type Tables = (T1, T2)

    private def from(): String = tables._1.name + ", " + tables._2.name

    def select[V1](row: Tables => Rep[V1]): Query[V1] = {
      Select(from(), where, row(tables).name)
    }

    def select2[V1, V2](row: Tables => (Rep[V1], Rep[V2])): Query[(V1, V2)] = {
      val (v1, v2) = row(tables)
      Select(from(), where, v1.name + ", " + v2.name)
    }

    def where[C1](predicate: Tables => Rep[C1]): From2[T1, T2] with WhereOps = {
      val pred = where.fold(predicate(tables).name)(_ + " and " + predicate(tables).name)
      new From2(tables, Some(pred)) with WhereOps
    }

    trait WhereOps { self: From2[T1, T2] =>

      def and[C1](predicate: Tables => Rep[C1]): From2[T1, T2] with WhereOps = {
        self.where(predicate)
      }
    }
  }

  def equal[T](a: Rep[T], b: Rep[T]): Rep[Boolean] =
    Rep(a.name + " = " + b.name)
}


