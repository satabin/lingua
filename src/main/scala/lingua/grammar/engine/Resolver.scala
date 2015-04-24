package lingua
package grammar
package engine

import lingua.grammar.{
  Rhs => RawRhs,
  Alternative => RawAlternative,
  Optional => RawOptional,
  Literal => RawLiteral,
  Symbol => RawSymbol,
  Abstract => RawAbstract
}

import scala.collection.mutable.Map

object Resolver {

  def resolve(g: Grammar, reporter: Reporter): (ProductionSym, Map[String, Sym[_ <: Type]]) = {

    def alreadyDefined(name: String, sym: Sym[_ <: Type], pos: Position) =
      reporter.error(f"name $name is already defined as ${sym.tpe} at ${sym.pos}", pos)

    // collect declared categories
    val table = Map.empty[String, Sym[_ <: Type]]
    for (cat @ Category(name, alias) <- g.categories)
      if (table.contains(alias)) {
        alreadyDefined(alias, table(alias), cat.pos)
      } else {
        table += (alias -> CategorySym(alias)(name, cat.pos))
      }
    // collect declared tags and groups
    for (tag @ Tag(name, alias, group) <- g.tags)
      if (table.contains(alias)) {
        alreadyDefined(alias, table(alias), tag.pos)
      } else {
        group match {
          case Some(group) =>
            val gsym =
              table.get(group) match {
                case Some(gsym @ GroupSym(_)) =>
                  gsym
                case Some(_) =>
                  alreadyDefined(group, table(group), tag.pos)
                  GroupSym("??")(tag.pos)
                case None =>
                  val sym = GroupSym(group)(tag.pos)
                  table += (group -> sym)
                  sym
              }
            table += (alias -> TagSym(alias, Some(gsym))(name, tag.pos))
          case None =>
            table += (alias -> TagSym(alias, None)(name, tag.pos))
        }
      }
    // collect declared productions
    for (prod @ Production(name, rhs) <- g.productions)
      if (table.contains(name)) {
        alreadyDefined(name, table(name), prod.pos)
      } else {
        table += (name -> UnresolvedProductionSym(name, rhs)(prod.pos))
      }
    for (prod @ Production(name, rhs) <- g.productions)
      resolveProduction(name, rhs, prod.pos, table, reporter)
    // resolve names in productions and build resolved rule tree
    table.get(g.start._1) match {
      case Some(prod @ ProductionSym(_, _)) =>
        (prod, table)
      case Some(prod @ UnresolvedProductionSym(name, rhs)) =>
        (resolveProduction(name, rhs, prod.pos, table, reporter), table)
      case Some(sym) =>
        reporter.abort(f"rule identifer expected but ${sym.tpe} found", g.start._2)
      case None =>
        reporter.abort(f"unknown rule identifer ${g.start._1}", g.start._2)
    }
  }

  private def resolve(rhs: RawRhs, pos: Position, table: Map[String, Sym[_ <: Type]], reporter: Reporter): Rhs = rhs match {
    case RawAlternative(alts) =>
      val alts1 = alts.foldLeft(Vector.empty[Vector[Rhs]]) { (altsAcc, rhss) =>
        val rhss1 = rhss.foldLeft(Vector.empty[Rhs]) { (rhsAcc, rhs) =>
          val rhs1 = resolve(rhs, pos, table, reporter)
          rhsAcc :+ rhs1
        }
        altsAcc :+ rhss1
      }
      Alternative(alts1)
    case RawOptional(rhss) =>
      val rhss1 = rhss.foldLeft(Vector.empty[Rhs]) { (rhssAcc, rhs) =>
        val rhs1 = resolve(rhs, pos, table, reporter)
        rhssAcc :+ rhs1
      }
      Optional(rhss1)
    case RawLiteral(lit) =>
      Literal(lit)
    case RawSymbol(name, tags) =>
      def resolveTag(name: String): Option[TagSym] =
        table.get(name) match {
          case Some(sym @ TagSym(_, _)) =>
            Some(sym)
          case Some(sym) =>
            reporter.error(f"name $name should be a tag but is of type ${sym.tpe}", pos)
            None
          case None =>
            reporter.error(f"unknown tag name $name", pos)
            None
        }
      table.get(name) match {
        case Some(cat @ CategorySym(_)) =>
          CategoryRef(cat, tags.flatMap(p => resolveTag(p._2).map((p._1, _))).toVector)
        case Some(UnresolvedProductionSym(name, rhs)) =>
          val sym = resolveProduction(name, rhs, pos, table, reporter)
          ProdRef(sym, tags.flatMap(p => resolveTag(p._2).map((p._1, _))).toVector)
        case Some(res @ ResolvingProductionSym(_)) =>
          ProdRef(res.resolved, tags.flatMap(p => resolveTag(p._2).map((p._1, _))).toVector)
        case Some(prod @ ProductionSym(_, _)) =>
          ProdRef(prod, tags.flatMap(p => resolveTag(p._2).map((p._1, _))).toVector)
        case Some(sym) =>
          reporter.error(f"name $name should be a category or production but is of type ${sym.tpe}", pos)
          ErrorRef
        case None =>
          reporter.error(f"unknown category or production name $name", pos)
          ErrorRef
      }
    case RawAbstract(name, tags) =>
      table.get(name) match {
        case Some(cat @ CategorySym(_)) =>
          def resolveTag(name: String): Option[SomeTag] =
            table.get(name) match {
              case Some(sym @ TagSym(_, _)) =>
                Some(sym)
              case Some(sym @ GroupSym(_)) =>
                Some(sym)
              case Some(sym) =>
                reporter.error(f"name $name should be a tag or group but is of type ${sym.tpe}", pos)
                None
              case None =>
                reporter.error(f"unknown tag or group name $name", pos)
                None
            }
          Abstract(cat, tags.flatMap(resolveTag(_)).toVector)
        case Some(sym) =>
          reporter.error(f"name $name should be a categiry but is of type ${sym.tpe}", pos)
          ErrorRef
        case None =>
          reporter.error(f"unknwon name $name", pos)
          ErrorRef
      }
  }

  private def resolveProduction(name: String, rhs: Seq[RawRhs], pos: Position, table: Map[String, Sym[_ <: Type]], reporter: Reporter): ProductionSym =
    table.get(name) match {
      case Some(sym @ ProductionSym(_, _)) =>
        sym
      case Some(UnresolvedProductionSym(_, rhs)) =>
        // resolve it
        val tmp = ResolvingProductionSym(name)(pos)
        table += (name -> tmp)
        val rhs1 = rhs.foldLeft(Vector.empty[Rhs]) { (rhsAcc, rhs) =>
          val rhs1 = resolve(rhs, pos, table, reporter)
          rhsAcc :+ rhs1
        }
        val sym = ProductionSym(name, rhs1)(pos)
        tmp.resolved = sym
        table += (name -> sym)
        sym
      case Some(sym) =>
        reporter.error(f"name $name should refer to a production but ${sym.tpe} found", pos)
        ProductionSym(name, Vector())(pos)
      case None =>
        reporter.error(f"unknown name $name", pos)
        ProductionSym(name, Vector())(pos)
    }

}
