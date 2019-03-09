package com.github.xplosunn.pql

import com.github.xplosunn.pql.PQL.Query.{Many, MaybeOne, One, Results}


object PQL {

  object Query {
    sealed trait Results
    sealed trait MaybeOne extends Results
    sealed trait One extends Results
    sealed trait Many extends Results
  }

  sealed trait Query[R <: Results, T] {
    def sql: String
  }

  sealed trait LimitExists
  sealed trait Limited extends LimitExists
  sealed trait Unlimited extends LimitExists

  case class Select[L <: LimitExists, R <: Results, T](select: String, from: String, where: Option[String], join: List[String] = List.empty, limit: Option[Int] = Option.empty) extends Query[R, T] {
    def sql: String = s"select $select from $from${where.fold("")(" where " + _)}${join.reduceOption(_ + " " + _).map(" " + _).getOrElse("")}${limit.map(" limit " + _).getOrElse("")}"

    def limit1(implicit ev: L =:= Unlimited): Select[Limited, MaybeOne, T] =
      Select[Limited, MaybeOne, T](select, from, where, join, Some(1))

    def limit(limit: Int)(implicit ev: L =:= Unlimited): Select[Limited, MaybeOne, T] =
      Select[Limited, MaybeOne, T](select, from, where, join, Some(limit))
  }

  case class Delete(table: String, where: Option[String]) extends Query[One, Int] {
    override def sql: String = s"delete from $table${where.fold("")(" where " + _)}"
  }

  abstract class Table(val name: String)

  case class OptTable[T <: Table](table: T) extends AnyVal {
    def map[B](f: T => Rep[B]): OptRep[B] = {
      OptRep(f(table).name)
    }

    def flatMap[B](f: T => OptRep[B]): OptRep[B] = {
      f(table)
    }
  }

  trait Output[T, Out] {
    def name: String
  }

  case class Rep[T](name: String) extends Output[T, T] {

    def >(other: T)(implicit num: Numeric[T]): Rep[Boolean] = {
      Rep(name + " > " + other.toString)
    }

    def <(other: T)(implicit num: Numeric[T]): Rep[Boolean] = {
      Rep(name + " < " + other.toString)
    }
  }

  case class OptRep[T](name: String) extends Output[T, Option[T]]

  def column[T](table: Table, name: String): Rep[T] = Rep[T](s"${table.name}.$name")

  def from[T <: Table](table: T): From[T] = From(table, table.name)

  def from2[T1 <: Table, T2 <: Table](t1: T1, t2: T2): From[(T1, T2)] = From((t1, t2), t1.name + ", " + t2.name)

  case class On[A, B](on: A => B)

  case class From[Tables](tables: Tables, from: String, where: Option[String] = None, join: List[String] = Nil) {

    def select[V1, O1](row: Tables => Output[V1, O1]) = {
      val v1 = row(tables)
      Select[Unlimited, Many, O1](v1.name, from, where, join)
    }

    def select2[V1, V2, O1, O2](row: Tables => (Output[V1, O1], Output[V2, O2])) = {
      val (v1, v2) = row(tables)
      Select[Unlimited, Many, (O1, O2)](v1.name + ", " + v2.name, from, where, join)
    }

    def delete(implicit ev: Tables <:< Table) = {
      Delete(from, where)
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
      join("join", otherTable, identity)

    def innerJoin[T2 <: Table](otherTable: T2): On[(Tables, T2) => Rep[Boolean], From[(Tables, T2)]] =
      join("inner join", otherTable, identity)

    def leftJoin[T2 <: Table](otherTable: T2): On[(Tables, T2) => Rep[Boolean], From[(Tables, OptTable[T2])]] =
      join("left join", otherTable, OptTable.apply)

    private def join[T2 <: Table, WT2](join: String, otherTable: T2, f: T2 => WT2): On[(Tables, T2) => Rep[Boolean], From[(Tables, WT2)]] = {
      val on: ((Tables, T2) => Rep[Boolean]) => From[(Tables, WT2)] = {
        cond =>
          From((tables, f(otherTable)), from, None, List(s"$join ${otherTable.name} on ${cond(tables, otherTable).name}"))
      }
      On(on)
    }
  }

  def equal[T](a: Rep[T], b: Rep[T]): Rep[Boolean] =
    Rep(a.name + " = " + b.name)
}


