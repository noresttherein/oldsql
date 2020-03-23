package com.hcore.ogre.mapping.support

import com.hcore.ogre.mapping.ComponentPath.{MappedComponent, DirectComponent, TypedComponentPath}
import com.hcore.ogre.mapping.MappingMorphism.{ValueMorphism, ComponentMorphism, StructuralIdentityMorphism, ValueIdentityMorphism}
import com.hcore.ogre.mapping.Mapping.{TypedMapping, MappingExtension, ReferenceContext}
import com.hcore.ogre.mapping.support.BaseMappings.MappingSupport
import com.hcore.ogre.mapping.{AnyMapping, MappingMorphism, Mapping}
import com.hcore.ogre.sql.{SQLReadForm, SQLWriteForm}

import scala.slick.jdbc.SetParameter



//implicits




trait MappingAdapter[V, M<:AnyMapping] extends Mapping[V] { adapter =>
	protected val adaptee :M

	type AdaptedComponent[X] = adaptee.Component[X]

	override def sqlName = adaptee.sqlName



	def canEqual(that :Any) = that.getClass == this.getClass

	override def equals(that :Any) = that match {
		case a:MappingAdapter[_, _] =>
			(a eq this) || (a.canEqual(this) && this.canEqual(that) && adaptee==a.adaptee) && contentsEqual(a)
		case _ => false
	}

	protected def contentsEqual(that :MappingAdapter[_, _]) = true

	override def hashCode = (getClass, adaptee).hashCode

	override def toString = s"~$adaptee"

	protected abstract class AdapterMorphism extends MappingMorphism[adapter.type, adaptee.type] {
		def source = adapter
		def target = adaptee
	}

}





object MappingAdapter {
	implicit def toAdaptee[E, T, M<:Mapping[T]](adapter :MappingAdapter[E, M]) :M = adapter.adaptee



	trait MappingProxy[V, M<:TypedMapping[V]] extends MappingAdapter[V, M] {
		override def modifiers: Seq[MappingExtension[V]] = adaptee.modifiers
		override def nullValue: Option[V] = adaptee.nullValue
//		override def columnDefinition = egg.columnDefinition

		protected trait ProxyMorphism extends AdapterMorphism with ValueIdentityMorphism[this.type, adaptee.type, V]
	}


	trait MappingMorphismAdapter[V, M<:AnyMapping] extends MappingAdapter[V, M] { adapter =>

		override def InsertParameters: SetParameter[V] = adaptedSetter(adaptee.InsertParameters)
		override def UpdateParameters: SetParameter[V] = adaptedSetter(adaptee.UpdateParameters)
		override def QueryParameters: SetParameter[V] = adaptedSetter(adaptee.QueryParameters)

		protected def adaptedSetter(setter :SetParameter[adaptee.ResultType]) :SetParameter[V] =
			SetParameter[V]{(s, params) =>
				val value = morphism.value(s) getOrElse {
					throw new IllegalArgumentException(s"can't adapt value $s of $this for target mapping $adaptee")
				}
				setter(value, params)
			}



//		protected def adaptedWrite(write :SQLWriteForm[T]) :SQLWriteForm[V] =
//			write.iflatMap(morphism.value.function)

		override def queryForm: SQLWriteForm[V] = adaptee.queryForm.iflatMap(morphism.value.function)
		override def updateForm: SQLWriteForm[V] = adaptee.updateForm.iflatMap(morphism.value.function)
		override def insertForm: SQLWriteForm[V] = adaptee.insertForm.iflatMap(morphism.value.function)


		protected def morphism :MappingMorphism[this.type, adaptee.type]
	}


//	trait AbstractMappingAdapter[V, T, M<:Mapping[T]] extends MappingAdapter[V, T, M] { adapter =>
//		protected def adapt[X](component :AdaptedComponent[X]) :Component[X]
//		protected def adapted(components :Seq[AdaptedComponent[_]]) :Seq[Component[_]] = components.map(adapt(_))
//
//	}



	trait AbstractMappingProxy[T, M<:TypedMapping[T]] extends MappingProxy[T, M] {
		override def InsertParameters: SetParameter[T] = adaptee.InsertParameters
		override def UpdateParameters: SetParameter[T] = adaptee.UpdateParameters
		override def QueryParameters: SetParameter[T] = adaptee.QueryParameters

		override def selectForm: SQLReadForm[T] = adaptee.selectForm
		override def queryForm: SQLWriteForm[T] = adaptee.queryForm
		override def updateForm: SQLWriteForm[T] = adaptee.updateForm
		override def insertForm: SQLWriteForm[T] = adaptee.insertForm
	}

	trait IsomorphicMappingAdapter[V, M<:AnyMapping] extends MappingAdapter[V, M] {
		type Component[X] >: adaptee.Component[X] <:Mapping[X]

		override def components :Seq[Component[_]] = adaptee.components
		override def subcomponents :Seq[Component[_]]  = adaptee.subcomponents

		override def columns :Seq[Component[_]]  = adaptee.columns
		override def selectable :Seq[Component[_]]  = adaptee.selectable
		override def querable :Seq[Component[_]]  = adaptee.querable
		override def updatable :Seq[Component[_]]  = adaptee.updatable
		override def insertable :Seq[Component[_]]  = adaptee.insertable
		override def generated :Seq[Component[_]]  = adaptee.generated
	}




	trait MappingAdapterSupport[V, M<:AnyMapping]
		extends MappingAdapter[V, M] with MappingSupport[V]
	{ adapter =>
		protected def adapt[X](component :AdaptedComponent[X]) :Component[X]
		protected def adapted(components :Seq[AdaptedComponent[_]]) :Seq[Component[_]] = components.map(adapt(_))

		override lazy val components = adapted(adaptee.components)
		override lazy val subcomponents = adapted(adaptee.subcomponents)
		override lazy val columns = adapted(adaptee.columns)

	}

	trait MappingProxySupport[V, M<:TypedMapping[V]] extends MappingAdapterSupport[V, M] with MappingProxy[V, M]







	trait DirectMappingAdapter[V, T, M<:Mapping[T]]
		extends MappingMorphismAdapter[V, M] with IsomorphicMappingAdapter[V, M]
	{
		type Component[X] = Mapping[X]

		override def components = Seq(adaptee)
		override def subcomponents = adaptee +: adaptee.subcomponents


		override implicit def \\[X](component: Component[X]): TypedComponentPath[this.type, component.type, X] = {
			if (adaptee==component) DirectComponent[this.type, adaptee.type, T](morphism)
			else {
				val cast = component.asInstanceOf[adaptee.Component[X]]
				DirectComponent[this.type, cast.type, X](morphism andThen (adaptee \\ cast).morphism)
			}
//			else DirectComponent[this.type, egg.type, T](morphism) :\ component.asInstanceOf[egg.Component[X]]
		}.asInstanceOf[TypedComponentPath[this.type, component.type, X]]



		@inline
		protected def componentMorphism = ComponentMorphism.supertype[adaptee.Component, Component]

		protected def morphism :MappingMorphism[this.type, adaptee.type]

		abstract class DirectMorphism extends AdapterMorphism {
			override def components = ComponentMorphism.supertype[adaptee.Component, Component]
		}
	}


	trait DirectMappingProxy[T, M<:Mapping[T]] extends DirectMappingAdapter[T, T, M] with AbstractMappingProxy[T, M] {

		override def scoutValue(ctx :ReferenceContext[this.type]) :Option[T] =
			adaptee.scoutValue(ctx :\ adaptee)

		override def assemble(values: Values): Option[T] =
			values.get(adaptee)
			//egg.assemble(values :\ egg)

		override def morphism = new DirectMorphism with ValueIdentityMorphism[this.type, adaptee.type, T]
	}







	trait MappingSubstitute[V, T, M<:Mapping[T]]
		extends MappingMorphismAdapter[V, M] with IsomorphicMappingAdapter[V, M]
	{
		type Component[X] = adaptee.Component[X]


		override implicit def \\[X](component: Component[X]): TypedComponentPath[this.type, component.type, X] =
			DirectComponent(morphism andThen (adaptee \\ component).morphism)

		protected trait SubstituteMorphism extends AdapterMorphism with StructuralIdentityMorphism[this.type, adaptee.type]
	}




	trait MappingImpostor[T, M<:Mapping[T]] extends MappingSubstitute[T, T, M] with AbstractMappingProxy[T, M] {

		override def assemble(values: Values): Option[T] =
			values.identical[adaptee.type](adaptee).result(adaptee)

		override def scoutValue(ctx :ReferenceContext[this.type]) :Option[T] =
			adaptee.scoutValue(ctx \\ MappedComponent(morphism))

//		override implicit def \\[X](component: Component[X]): TypedComponentPath[this.type, component.type, X] =
//			DirectComponent(morphism andThen (egg \\ component).morphism)

		def morphism = new SubstituteMorphism with ProxyMorphism //with StructuralIdentityMorphism[this.type, egg.type]

	}


	trait GenericMappingSubstitute[V, M<:AnyMapping] extends IsomorphicMappingAdapter[V, M]
	{ substitute =>
		type Component[X] = M#Component[X]


		override implicit def \\[X](component: Component[X]): TypedComponentPath[this.type, component.type, X] =
			DirectComponent(morphism andThen (adaptee \\ component.asInstanceOf[adaptee.Component[X]]).morphism.asInstanceOf[MappingMorphism[M, component.type]])

		trait SubstituteMorphism extends StructuralIdentityMorphism[this.type, M] {
			def source = substitute
			def target = adaptee
		}

		override def InsertParameters: SetParameter[V] = adaptedSetter(adaptee.InsertParameters)
		override def UpdateParameters: SetParameter[V] = adaptedSetter(adaptee.UpdateParameters)
		override def QueryParameters: SetParameter[V] = adaptedSetter(adaptee.QueryParameters)

		protected def adaptedSetter(setter :SetParameter[adaptee.ResultType]) :SetParameter[V] =
			SetParameter[V]{(s, params) =>
				val value = morphism.value(s) getOrElse {
					throw new IllegalArgumentException(s"can't adapt value $s of $this for target mapping $adaptee")
				}
				setter(value.asInstanceOf[adaptee.ResultType], params)
			}



		//		protected def adaptedWrite(write :SQLWriteForm[T]) :SQLWriteForm[V] =
		//			write.iflatMap(morphism.value.function)

		override def queryForm: SQLWriteForm[V] = adaptee.queryForm.iflatMap(morphism.value.function.asInstanceOf[V=>Option[adaptee.ResultType]])
		override def updateForm: SQLWriteForm[V] = adaptee.updateForm.iflatMap(morphism.value.function.asInstanceOf[V=>Option[adaptee.ResultType]])
		override def insertForm: SQLWriteForm[V] = adaptee.insertForm.iflatMap(morphism.value.function.asInstanceOf[V=>Option[adaptee.ResultType]])


		protected def morphism :MappingMorphism[this.type, M]
	}


	trait GenericMappingImpostor[M<:AnyMapping] extends GenericMappingSubstitute[M#ResultType, M] /*with AbstractMappingProxy[M#ResultType, M]*/ {

		override def assemble(values: Values): Option[M#ResultType] =
			values.morph(morphism).result(adaptee)

		override def scoutValue(ctx :ReferenceContext[this.type]) :Option[M#ResultType] =
			adaptee.scoutValue(ctx \\ MappedComponent(morphism.asInstanceOf[MappingMorphism[this.type, adaptee.type]]))

		override def InsertParameters: SetParameter[M#ResultType] = adaptee.InsertParameters.asInstanceOf[SetParameter[M#ResultType]]
		override def UpdateParameters: SetParameter[M#ResultType] = adaptee.UpdateParameters.asInstanceOf[SetParameter[M#ResultType]]
		override def QueryParameters: SetParameter[M#ResultType] = adaptee.QueryParameters.asInstanceOf[SetParameter[M#ResultType]]

		override def selectForm: SQLReadForm[M#ResultType] = adaptee.selectForm
		override def queryForm: SQLWriteForm[M#ResultType] = adaptee.queryForm.asInstanceOf[SQLWriteForm[M#ResultType]]
		override def updateForm: SQLWriteForm[M#ResultType] = adaptee.updateForm.asInstanceOf[SQLWriteForm[M#ResultType]]
		override def insertForm: SQLWriteForm[M#ResultType] = adaptee.insertForm.asInstanceOf[SQLWriteForm[M#ResultType]]


		def morphism = new SubstituteMorphism {
			val value = ValueMorphism.identity[M#ResultType]
		}
	}



	trait FullMappingAdapter[V, T, M<:Mapping[T]] extends MappingAdapterSupport[V, M] {
		adapter =>

		type Component[X] <: ComponentProxy[X]

		trait ComponentProxy[X] extends Mapping[X] {
			def adaptedComponent :AdaptedComponent[X]

			def adaptedSubcomponent[S](subcomponent :Component[S]) :AdaptedComponent[X]#Component[S]

			def toAdaptedComponent[C<:adapter.AdaptedComponent[X]] :ComponentMorphism[Component, C#Component]
		}

		protected abstract class AbstractComponent[X](val adaptee :AdaptedComponent[X])
			extends MappingProxy[X, AdaptedComponent[X]] with ComponentProxy[X]
		{
			final def adaptedComponent = adaptee
			
		}

		trait ComponentImpostor[X]
			extends ComponentProxy[X] with MappingImpostor[X, AdaptedComponent[X]]
		{
			def adaptedSubcomponent[S](subcomponent: Component[S]) = subcomponent

			def toAdaptedComponent[C<:adapter.AdaptedComponent[X]] :ComponentMorphism[Component, C#Component] =
				ComponentMorphism.identity[Component].asInstanceOf[ComponentMorphism[Component, C#Component]]
		}
		
		trait FullComponentProxy[X]
			extends ComponentProxy[X] with FullMappingProxy[X, AdaptedComponent[X]]
		{
			override def toAdaptedComponent[C <: adapter.AdaptedComponent[X]]: ComponentMorphism[Component, C#Component] =
				ComponentMorphism.identity[Component].asInstanceOf[ComponentMorphism[Component, C#Component]]

			override def adaptedSubcomponent[O](subcomponent: Component[O]): adapter.AdaptedComponent[X]#Component[O] =
				subcomponent.adaptedComponent
		}

	}



	trait FullMappingProxy[E, M<:Mapping[E]]
		extends FullMappingAdapter[E, E, M] with MappingProxySupport[E, M]
	{ adapter =>

		/** An object rather than a val as lazy initialization is important. */
		protected object morphism extends ProxyMorphism {
			override val components =
				ComponentMorphism.cached[AdaptedComponent, Component](adaptee.subcomponents, adapt(_))
//			override val components = fromAdapted //ComponentMorphism.homomorphism[adaptedMapping.Component, adapter.Component](componentMapping)
		}


		override def assemble(values: Values): Option[E] =
			values.morph(morphism).result(adaptee)

		override def scoutValue(ctx :ReferenceContext[this.type]) :Option[E] =
			adaptee.scoutValue(ctx \\ MappedComponent(morphism))

//		protected lazy val fromAdapted =
//			ComponentMorphism.cached[AdaptedComponent, Component](egg.subcomponents, adapt(_))


		override implicit def \\[T](component: Component[T]): TypedComponentPath[this.type, component.type, T] = {
			val adaptedComponent = component.adaptedComponent
			val adaptedMorphism = (adaptee \\ adaptedComponent).morphism
			val subcomponentMapping = component.toAdaptedComponent[adaptedComponent.type] andThen
				adaptedMorphism.components andThen morphism.components

			DirectComponent(this :this.type)(component)(adaptedMorphism.value, subcomponentMapping)
		}
	}
	




}

