package com.hcore.ogre.hoard

import com.hcore.ogre.mapping.ties.Pathfinder
import com.hcore.ogre.mapping.{ComponentPath, Mapping}
import com.hcore.ogre.model.Restriction
import com.hcore.ogre.model.Restriction._
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.sql.SQLFormula.{BooleanFormula, ComponentFormula, PathFormula, SeqFormula}
import com.hcore.ogre.sql.{From, SQLFormula, SQLForm, SQLTypes}


class MappingFilter[M<:Mapping[T], T](val mapping :M, pathfinder :Pathfinder[T, Any], types :SQLTypes=SQLForm) {

//	import pathfinder.mapping
	val source = From(mapping)


	def references(restriction :Restriction[T]) :Seq[PropertyChain[T, Any]] = {
		def rec(expr :Restriction[T], acc :Seq[PropertyChain[T, Any]]=Seq()) :Seq[PropertyChain[T, Any]] = expr match {
			case Conjunction(restrictions) =>
				(acc /: restrictions)((props, c) => rec(c, props))
			case Disjunction(restrictions) =>
				(acc /: restrictions)((props, c) => rec(c, props))
			case Restriction.Equality(left, right) =>
				Seq(left, right).collect { case Restrictive.Property(prop) => prop } ++: acc
			case Membership(left, right) =>
				(left +: right.toSeq).collect { case Restrictive.Property(prop) => prop } ++: acc
			case _ => Seq()
		}
		rec(restriction)
	}



	def apply(restriction :Restriction[T]) :BooleanFormula[From[M]] = restriction match {
		case Restriction.True => SQLFormula.True

		case Restriction.False => SQLFormula.False

		case Conjunction(restrictions) => SQLFormula.And(restrictions.map(apply(_)))

		case Disjunction(restrictions) => SQLFormula.Or(restrictions.map(apply(_)))

		case Equality(left, right) => SQLFormula.Equality(expression(left), expression(right))

		case Membership(left, values) =>
			SQLFormula.In(expression(left.asInstanceOf[Restrictive[T, Any]]), SeqFormula(values.toSeq.map(v => expression(v.asInstanceOf[Restrictive[T, Any]]))))
//		case TypeRestriction()
//		case SQLRestriction()
//		case ForAll
//		case Exists
		case _ => throw new IllegalArgumentException(s"Don't know how to create e filter for $restriction")


	}

	def expression[X](constrainee :Restrictive[T, X]) :SQLFormula[From[M], X] = constrainee match {
		case Restrictive.Property(property) =>
			PathFormula[From[M], M, Mapping[X]](source.last, pathfinder(property).cast[M, Mapping[X]])

		case Restrictive.Literal(v) =>
			val sqlType = types.get(v) getOrElse SQLForm.Unknown[X]()
				SQLFormula.BoundParameter(v)(sqlType)

		case Restrictive.Self(tpe) =>
			ComponentFormula[From[M], M, M](source.last, ComponentPath.self(mapping)).asInstanceOf[SQLFormula[From[M], X]]
	}
}



object MappingFilter {
	

}


