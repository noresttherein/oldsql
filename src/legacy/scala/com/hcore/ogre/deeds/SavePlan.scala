package com.hcore.ogre.deeds

import com.hcore.ogre.mapping.Mapping
import com.hcore.ogre.sql.RowSource.WithParam
import com.hcore.ogre.sql._


trait SavePlan[M<:Mapping[E], E]


class SaveFormula[M<:Mapping[E], E](val target :M, val formula :SQLFormula[Nothing WithParam E, E]) extends SavePlan[M, E]



class InsertFormula[M<:Mapping[E], E](target :M, override val formula :SQLFormula[Dual WithParam E, E]) extends SaveFormula[M, E](target, formula)


class UpdateFormula[M<:Mapping[E], E](target :M, override val formula :SQLFormula[From[M] WithParam E, E]) extends SaveFormula[M, E](target, formula)


