package com.hcore.ogre.mapping.support

import com.hcore.ogre.mapping.{Mapping, TypedColumn}
import com.hcore.ogre.mapping.ComponentPath.{DirectComponent, TypedComponentPath}
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.SQLForm.{UnitForm, NothingForm}
import com.hcore.ogre.sql.{SQLForm, SQLReadForm, SQLWriteForm}

import scala.slick.jdbc.SetParameter


//implicits
import extensions._



object BaseMappings {
	import Mapping._
	import Mapping.MappingExtension._


	trait UniversalMapping[E] extends Mapping[E] {
		type Component[X] = Mapping[X]
		//		type Column[X] = TypedColumn[X]
	}


	trait MappingSupport[E] extends Mapping[E] {
		lazy val querable = columnsWithout(NoQuery)
		lazy val selectable = columnsWithout(NoSelect)
		lazy val updatable = columnsWithout(NoUpdate)
		lazy val insertable = columnsWithout(NoInsert)
		lazy val generated = columnsWith(AutoGen)

		lazy val InsertParameters: SetParameter[E] = SetParameters(this)(_.InsertParameters, insertable)
		lazy val UpdateParameters: SetParameter[E] = SetParameters(this)(_.UpdateParameters, updatable)
		lazy val QueryParameters: SetParameter[E] = SetParameters(this)(_.QueryParameters, querable)

		lazy val selectForm: SQLReadForm[E] = MappingReadForm.selectForm(this)
		lazy val queryForm: SQLWriteForm[E] = MappingWriteForm.queryForm(this)
		lazy val updateForm: SQLWriteForm[E] = MappingWriteForm.updateForm(this)
		lazy val insertForm: SQLWriteForm[E] = MappingWriteForm.insertForm(this)
	}



	trait AbstractEmptyMapping[E] extends Mapping[E] {

		override def components = Seq()
		override def subcomponents = Seq()
		override def columns = Seq()
		override def querable = Seq()
		override def selectable = Seq()
		override def updatable = Seq()
		override def insertable = Seq()
		override def generated = Seq()

		override def InsertParameters = SetParameters.Empty
		override def UpdateParameters = SetParameters.Empty
		override def QueryParameters = SetParameters.Empty

//		override def selectForm: SQLReadForm[E] = ???
		override def queryForm: SQLWriteForm[E] = UnitForm.iflatMap(_ => None)
		override def updateForm: SQLWriteForm[E] = UnitForm.iflatMap(_ => None)
		override def insertForm: SQLWriteForm[E] = UnitForm.iflatMap(_ => None)
	}


	trait EmptyMapping[E] extends AbstractEmptyMapping[E] {
		type Component[T] = Nothing

		override def selectForm: SQLReadForm[E] = NothingForm

		override def assemble(values: Values): Option[E] = None

		override implicit def \\[T](component: Component[T]): TypedComponentPath[this.type, component.type, T] =
			throw new UnsupportedOperationException(s"$this \\ $component: EmptyMapping doesn't have components!")
	}


	trait SelectOnlyMapping[E] extends Mapping[E] {
		override def modifiers = Seq(NoUpdate, NoInsert)

		override def InsertParameters: SetParameter[E] = SetParameters.Empty
		override def UpdateParameters: SetParameter[E] = SetParameters.Empty

		override def columns = selectable

		override def updatable = Seq()
		override def insertable = Seq()
		override def generated = Seq()
	}



//	trait FlatMappingSupport[E] extends MappingSupport[E] {
//		def components = columns
//		def subcomponents = columns
//
//		override implicit def \\[X](component: Component[X]): TypedComponentPath[this.type, component.type, X] =
//			DirectComponent()
//	}


	trait FlatMapping[E] extends UniversalMapping[E] with MappingSupport[E] {
		def components = columns
		def subcomponents = columns
	}



	class ColumnSeqMapping[E](val columns :Seq[TypedColumn[_]], compose :Seq[_] => E, decompose :E=>Seq[_])
		extends FlatMapping[E]
	{
		override def modifiers = Seq()

		override def apply(values: Values): E =
			compose(columns.map(c => values(c)))

		override def assemble(values: Values): Option[E] =
			(Option(Seq[Any]()) /: columns.map(c => values.get(c))){
				(acc, v) => acc.flatMap(seq => v.map(_ +: seq))
			}.map(seq => compose(seq.reverse))

		private def pick[T](column :Component[T])(value :E) :T =
			columns.indexOf(column).providing(_>=0).map(decompose(value)(_).asInstanceOf[T]) getOrElse {
				throw new IllegalArgumentException(s"can't pick value for $column from $value: it is not a part of $this")
			}

		override implicit def \\[T](component: Component[T]): TypedComponentPath[this.type, component.type, T] =
			DirectComponent(this :this.type)(component)(ValueMorphism.homomorphism(pick(component) _), ComponentMorphism.empty[Component])

		override def toString = columns.mkString("ColumnSeq(", ",", ")")
	}




}
