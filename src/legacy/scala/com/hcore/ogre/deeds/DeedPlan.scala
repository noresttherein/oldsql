package com.hcore.ogre.deeds

import com.hcore.ogre.mapping.Mapping


trait DeedPlan[M<:Mapping[E], E]



trait InsertPlan[M<:Mapping[E], E] extends DeedPlan[M, E]


trait UpdatePlan[M<:Mapping[E], E] extends DeedPlan[M, E]


trait DeletePlan[M<:Mapping[E], E] extends DeedPlan[M, E]



trait SineQuaNon[M<:Mapping[E], E] extends InsertPlan[M, E] with UpdatePlan[M, E] with DeletePlan[M, E]
