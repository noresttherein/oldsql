# OldSQL
_(old school)_

OldSQL is - or, rather, will be - an object-relational mapper for Scala built around principles of explicit statement 
of intent and transparent, modular architecture.


## About
Object-oriented application model and a relational database are very distinct abstractions. Attempts to add persistence
on the application level, completely abstracting over the relational model underneath are doomed from the start.
A programmer must be familiar with SQL and understand how any given ORM works anyway; one of the more annoying 
situations is knowing what query or statements one wants to produce, and having to figure out how to make their tool
produce the desired output. 

All object relation mappers need to strike a balance between ease of use, and expressiveness. 
On one hand, there is a desire for making standard use cases as simple as possible, while on the other expressive power.
Streamlining the interface to achieve a gentle learning curve and low boilerplate comes at the cost of flexibility,
especially with regard to the mapping part. Huge time savings when implementing the 80% of the application 
are of little help if those gains are lost when trying to implement the remaining non-standard 20% of requirements. 
OldSQL cuts this Gordian knot by providing two set of APIs:
  1. High abstraction, model level interface, defined in terms of application's model classes, but still being
     expressive enough to support the majority of simpler applications;
  1. Low abstraction, metamodel interface, offering a WYSIWYG way of building SQL queries and statements 
     in a type safe fashion.

What does it look like?


### Model
The domain model consists of any application classes, although some of the functionality, like dirty checking, 
assumes that they are immutable. One restriction is that relationships require wrapping the referenced entity class
in a `Kin` reference, for example:

    case class Spellbook(owner :Kin[Mage], spells :Kin[Set[Spell]])

`Kin` is like `Option`, in that it may be `Present` or `Absent`, but offers additional functionality:
  1. Possibility of distinguishing, among `Absent`, between `Nonexistent` - for example, a `NULL` foreign key -
     and `Missing` - known to exist in the database, but not loaded together with the owning entity.
  2. All `Kin` properties of classes loaded through the API carry additional information, defining exactly
     which entity/entities are referenced, and sufficient to load them based on that `Kin` instance alone.
     This can be something simple like table name and primary key, for a `Kin` mapped to a foreign key, for objects
     loaded directly from the database, but `Kin` can also be created based on any expressible query, 
     allowing the application to update relationships without loading referenced entities themselves. 
     At the same time, a special `Unknown` instance can be used by the application when in need of updating 
     only the properties of the owning entity, without knowledge about its relationships.
  3. `Kin` may be used not only to reference other entities, but also for properties which we may want to avoid loading
     every time, like `BLOB` columns.

Best practices of shortest possible transaction spans mean that lazy loading, a cornerstone feature of JPA 
and other solutions, is rarely, if ever, really used. This essentially makes lazily loaded references null pointers 
of the 21st century, hence the decision to adapt a similar solution. With some determination, it is possible
to replace `Kin` with an arbitrary class with a `KinLike` type class, although some functionality may be unavailable.


### Metamodel
Metamodel classes are definitions of table schema, and how tables are mapped to application model classes. 
It is not dissimilar to Typesafe Slick's table mappings, although with some 'ease of life' features. 
We consider the 'class per table, column per property' to be evil: a user doesn't have a street or area code - 
they may have an address and a phone number, though. For this reason metamodel is component oriented and polymorphic:
both a single column, and a table mapping implement the same trait `Meta`, as well as metamodel 
for any dependent objects among properties of the mapped entity.

    //Model classes
    case class Level(toInt :Int)
    object Level { //allows the use of Level as a column type
        implicit val form = ColumnForm.map("Level")(Level.apply)(_.toInt)
    }
    case class Familiar(name :String, species :String)
    case class Mage(id :PK, name :String, level :Level, familiar :Familiar, spellbook :Kin[Spellbook], tutor :Kin[Mage])
    case class Spellbook(id :PK, spells :Kin[Iterable[Spell]])
    case class Spell(id :PK, level :Level, name :String, inventor :Kin[Mage])

    //Type parameter O stands for Origin - a marker type shared by a metamodel object with all of its components,
    // allowing linking them to their owner, and their use as SQL expressions. A long story.
    class Mages[O] extends MetaFrame[Mage, O] {
        val id    = column(_.id)   //Types are inferred, column name based on the property name
        val name  = column(_.name) // and configured naming strategy, but both can be also specified explicitly.
        val level = column(_.level)
        //Could be also defined as an independent, reusable class
        object familiar extends ComponentFrame(_.familiar) {
            val name    = column(_.name)
            val species = column(_.species)
            override def construct(implicit pieces :Pieces) = Familiar(name, species)
        }
        val spellbook = fk(_.spellbook)(_.id)
        val tutor     = fk(_.tutor)(_.id)

        //Note how metamodel objects are used in place of model objects: an implicit value 
        // of type Pieces=ComponentValues[Mage, O] enables an implicit conversion `Component[T] => T`.
        override def construct(implicit pieces :Pieces) = 
            Mage(id, name, level, familiar, spellbook, tutor)
    }

    class Spellbooks[O] extends MetaFrame[Spellbook, O] {
        val id = column(_.id)
        val spells = kin(_.spells, ListedSpells, _._1, _._2)  
        override def construct(implicit pieces :Pieces) = Spellbook(id, spells)
    }
    
    class ListedSpells[O] extends ColumnJoinTableMeta[Kin[Spellbook], Kin[Spell], O](_.id, _.id)

    class Spells[O] extends MetaFrame[Spell, O] {
        val id       = column(_.id)
        val name     = column(_.name)
        val level    = column(_.level)
        val inventor = column(_.inventor)
        override def construct(implicit pieces :Pieces) = Spell(id, name)
    }

    //A good practice is to define all tables as lazy val or object, to avoid initialization cycle issues
    // due to foreign keys. Being implicit allows omitting the table parameter in foreign key deinitions.
    implicit lazy val Mages = Table("Mages", new Mages)
    implicit lazy val Spellbooks = Table("Spellbooks", new Spellbook)
    implicit lazy val Spells = Table("Spells", new Spell)
    implicit lazy val Spells = Table("Spells", new Spells)
    implicit lazy val ListedSpells = JoinTable("ListedSpells", Spellbooks, _.id, Spells, _.id) 

Alternative base classes exist, depending on the desired levels of freedom vs convention, including a type level,
programmatic interface. In the future, it will be possible to avoid the definition of the metamodel classes, 
and use JPA annotations instead, if the application does not need to use explicit SQL queries and low level operations.


### Domain model API
Domain model API consists of variants of largely source compatible interfaces, which vary in implicit method parameters,
depending on desired level of isolation from the SQL/metamodel layer, and chosen model of transaction passing.
They are relatively simple, yet expressive, and meant to substitute `DataResource/DAO` classes with fixed, 
use-case specific methods. A minimal interface works in presence of implicit `Meta` definitions; 
the main query method has the form of:

    Hoard[Mage](_.level >= 20)(_.spellbook.spells, _spellbook.spells.all.tutor, _.tutor)

where the first parameter group contains the query filter as a function of the main retrieved entity - `Mage`,
while the second parameter group is a variable argument list with functions of said entity returning `Kin` for
relations which should be fetched together with the main entity. The method will create and execute as many
database queries as necessary to return the required entities - in the example above, current implementation
will first run a query `FROM Mages JOIN Spellbooks`, and then `FROM Spellbooks JOIN ListedSpells JOIN Spells`.
Any entities which need to be included in an underlying query in order to implement the filter condition
are loaded automatically, and all `Kin`, whose referenced entities are loaded for any reason, by any query executed
by the method, will be initialized. The filter can also use some of `Iterable` methods which can be directly translated  
to SQL, like `exists` and `forall`:

    Hoard[Mage](_.spellbook.spells.exists(_.level >= 20))

Update and inserts are explicit - there is no concept of 'persistent' and 'transient' states, but cascades by default
to all relationships:

    Hoard.save(mage)

This however can be controlled in several ways:
  1. By *buffing* the foreign key with one of `NoCascade` `Buff` types;
  2. By manually setting the `Kin` for relationships which should not cascade to `Unkown`;
  3. By specifying the cascaded relationships explicitly, like with queries:

    Hoard.save(mage)(_.spellbook, _.tutor)


### Meta model API
The metamodel API consists of a dsl closely resembling SQL, but with some additional features. The changes, 
however, should not detract from its WYSIWYG nature and give the programmer full control.

Two types are central to the design:
  1. `Clause` is an encoded on type level list of relation mappings, 
     a generalization of *from* clause, encompassing also *FROM ... GROUP By*.
  2. `SQL[F, S, V]` is an SQL expression, or a higher abstraction injected into SQL. Type parameter `F <: Clause`
     specifies in context of what *from* clauses the expression can be used, `S` is a phantom type marking
     aggregate expressions, while `V` is the Scala type to which the expression is mapped. 
     The DSL supports also multi-column expressions.


The creation of any *SELECT* starts with creating a *from* clause:

    //some types printed in the examples have been replaced with appropriate aliases for clarity and brevity
    > val from = From(Mages) leftJoin Spellbooks on (_.spellbook.key = _.id) where ( _[Mages].level >= 20.?)
    > val from :From[Mages] LeftJoin Spellbooks = from Mages@1 leftJoin Spellbooks@0 on (Mages@1.spellbook.key = Spellbooks@0.id) where (Mages@1.level >= ?20)

Here, `_[Mages]` was a getter returning the mapping `Mages[Clause AndFrom Mages Join Spellbooks]` (from the first table).
There is nn implicit conversion from any `M[O] <: TypedMapping[S, O]` to `SQLExpression[O, Single, S]`
Here, the type parameter specifies both that the `level` is a column of the first table, and that, as an expression,
it can be used only in context of a *from* clause ending with those two tables. 

    //compile error, because the where clause is SQLBoolean[Clause AndFrom Spells, Single], 
    // not SQLBoolean[Clause AndFrom Mages Join Spellbooks, Single] 
    > val error = from where (From(Spells).level >= 20)

It is possible to convert an expression from one `Clause` to another, for example:

    > from where (From(Mages).last.expand[From[Mages] Join Spellbooks].level >= 20.? 

Tables of a `Clause` can be accessed either by specifying their `Meta` type, as above, but also by aliases, 
subject (entity) type, or index:

    > From(Mages) as "mages" join Spellbooks as "spellbooks" on (_("mages").spellbook.key == _(1).id) where (_[Mage].level >= 20.?)
    > val res0 :From[Mages] As "mages" Join Spellbooks As "spellbooks" = \\
        from Mages@1 as mages join Spellbooks@0 as spellbooks where ... (same as before)

In the examples above, `20.?` was an expression for a *bound* `JDBC` parameter, i.e. an expression of known value, 
translated to an `?` which is set before execution to `20`. It is however also possible to introduce 
*unbound* parameters, whose values are not known, and only specified externally when the statement is executed:

    > val paramFrom = From(Mages) join Spellbooks joinParam ("level" ?: Int) where (_[Mages].level >= _("level"))
    > val paramFrom :From[Mages] Join Spellbooks JoinParam ParamRelation[Int]#Param As "level" = \\
        from Mages@2 join Spellbooks@1 joinParam "level":Int@0 as "level" where (Mages@2.level >= "level":Int@0)

Note that the parameter is introduced as a pseudo join, so you can treat it like any other `JoinedRelation`, 
for example access it by type: `_[Level]`. 

Both SQL expressions and parameter relations may consist of multiple columns:

    > From(Mages) joinParam ?:[Familiar] on (_.familiar == _)

While it is possible to write `Select(expression) from fromClause`, 
it is more convenient to append `select` after `from`:

    //An implicit conversion from (SQL[F, S, Mage], SQL[F, S, Spellbook]) to SQL[F, S, (Mage, Spellbook)] in play:
    > val select = from select (_[Mages] -> _[Spellbooks])
    > val select :SelectSQL[Clause, (Mage, Spellbook)] = \\
        select (Mages@1, Spellbooks@0.id) \\
        from Mages@1 leftJoin Spellbooks@0 on (Mages@1.spellbook.key = Spellbooks@0.id) where (Mages@1.level >= ?20)

Note that `SelectSQL` above was parameterized with `Clause` - it is because it also is an SQL expression, and, 
in this case, a *ground* one - not depending on any external relations (because it refers only to tables 
in its *from* clause). This is not a requirement, and ''dependent selects'' can access tables
of all enclosing ''selects'':

    //A clause of a dependent select is a linearization of its proper from clause, and that of the enclosing select:
    > val subclause = from subselect (_[Spellbooks].spells) where (_[Spells].level >= 20.?)
    > val subclause :From[Mages] LeftJoin Spellbooks Subselect ListedSpells Join Spells = \\
        from Mages@3 leftJoin Spellbooks@2 on (Mages@3.spellbook.key = Spellbooks@2.id) where (Mages@1.level >= ?20) \\
        subselect ListedSpells@1 on (Spellbooks@2.id = ListedSpells@1._1.key) join Spells@0 on (ListedSpells@1._2.key = Spells@0.id) \\
        where (Spells@0.level >= ?20)

    //t from (_[Spellbooks.spells) == subclause:
    > val from1 = from where { t => (t from (_[Spellbooks].spells) select (_[Spells].id)).exists }
    > val from1 :From[Mages] LeftJoin Spellbooks = \\
        from Mages@1 leftJoin Spellbooks@0 on (Mages@1.spellbook.key = Spellbooks@0.id) \\
        where (Mages@1.level >= ?20) and exists(\\
            select Spells@0.id from ListedSpells@1 join Spells@0 on (ListedSpells@1._2.key = Spells@0.id) where (Spellbooks@2.id = ListedSpells@1._1.key)\\
        ) 
    //or simply:
    > val from2 = from where (_[Spellbooks].spells.exists(_.level >= 20.?))

As mentioned before, *group by* clause is also treated like a *from* clause, except now its relations are any
grouping expressions:

    //Familiar is an object; Familiar.withOrigin[O] =:= Familiar.type { type Origin = O }):
    > val grouped = From(mages) groupBy (_.last.familiar)
    > val grouped :From[Mages] GroupBy Familiar.withOrigin = from Mages@0 groupBy Mages@0.familiar
    
    //_[Familiar] now refers to the grouping relation in GroupBy Familiar.withOrigin.
    //_.ungrouped refers to the grouped FROM clause: From(Mages).relations, as in previous examples
    > grouped select (_[Familiar] -> _.ungrouped[Mages].level.avg)
    > val res1 :SelectSQL[Clause, (Familiar, Int)]

    //It is of course possible to group not only by a column/component, but any expression
    //(type ByVal[F, X] = F By [O]SQLMeta[X, O], where SQLMeta is a synthetic adapter of an SQL expression to Meta): 
    > val byFamiliarAndLevelTier = grouped by (_.level / 10 * 10)
    > val byFamiliarAndLevelTier :From[Mages] GroupBy Familiar.withOrigin ByVal Int = \\
        from Mages@0 groupBy Mages@0.familiar by (Mages@0.level / 10 * 10)

Aggregate functions can be used only either if their argument expression is grounded in a `Clause` 
under (that is, left type parameter of) a `GroupByClause`, or if the whole ''select'' is aggregated.
At the same time, it is impossible to refer to individual rows of an aggregated table:

    //compile error: impossible to treat From[Mages] both as aggregated (avg(level)) and non aggregated (name)
    > From(Mages) select (_.level.avg -> _.name)

    //compile error: table Mages can be used only inside an argument to an aggregate function.
    > grouped select (_.ungrouped[Mages].level)

In case you wondered: `GroupBy` works only as far left as the first `Subselect` (or `From`, or `Dual`): tables
before `Subselect` pseudo join (that is, tables from enclosing ''selects'') can be accessed normally.

`With` clause is supported; simple clauses as a part of the DSL, while mutually recursive ''common table expressions'' 
are supported transparently - a ''common table expression'' relation can be used just like any other `Table`, 
and the generated SQL will automatically include a `With` clause if any such tables are used anywhere 
in the query expression.

    > With(From(Mages) select (_.level.max))(maxLevel => From(Mages) where (_.level == maxLevel) select *)
    > val res0 :From[Mages] = with cte0 as (from Mages@0 select (max(Mages@0.level))) select Mages@0 from Mages@0

    > lazy val inventors :CTE[Mages] = "mages" as (\\
        From(Mages) select * where (_.name == "Neera") 
        unionAll From(spells) select (_.inventor) \\
      ) \\
      lazy val spells :CTE[Spells] = "spells" as (From(inventors) join (_.spellbook) join (_.spells) select (_.last)) \\
      From(spells) select (_.name)
        

Both `SQL` and `Clause` class hierarchies are designed to be extensible, with all operations being polymorphic 
in an object-oriented fashion, so if an expression type specific to a certain DBMS is missing, it is easy to
implement it on the application side. Similarly, `LateralJoin` and functions returning cursors are not yet implemented,
but `Clause` interface can support them.

On the other side of things, *insert*, *update* and *delete* have a richer grammar than their SQL counterparts,
due to various ways to pass parameters, and streamlining of typical cases. Examples include:

  - Delete:
        
        //An explicit delete statement with bound parameters:
        > Delete from Mages where (_.name ==? "Radagast" || _.name == "Gandalf".?)
        > val res0 :Delete[Any, Mages, Int] = ...
    
        //Delete of the specified entities based on their primary keys:
        > Delete(gandalf, radagast) from mages
        > val res1 :Delete[Any, Mages, Int] = ...
    
        //A JDBC batch using bound parameters:
        > Delete(Mages)(gandalf, radagast)
        > val res2 :Deletes[Any, Mages, Int]
    
        //An explicit delete statement with unbound parameters:
        > Delete(Mages)[Familiar]["name", String] where (_.familiar == _[Familiar]) or (_.name = _("name"))
        > val res3 :Delete[(Familiar, String), Mages, Int] = ...
    
        //Delete of an entity by primary key, parameterized by the entity type 
        > Delete one Mages 
        > val res4 :Delete[Mage, Mages, Int] = ...
    
        //JDBC batch of Delete one Mages:
        > Delete many Mages
        > val res5 :Deletes[Iterable[Mage], Mages, Int] = ...

  - Update:
        
        //An explicitly written update using bound parameters:
        > Update table Mages set (_.level += 1) set (_.familiar :=? Familiar("Eyes", "spider")) where (_.name == "Morrigan")
        > val res0 :Update[Any, Mages, Int] = ...
        
        //A bound parameter entity update an override set clause
        > Update(Mage(42, "Mereth", 5, Familiar("Pratt", "rat), Unknown, Unknown)) in Mages supplant \\
            (_.tutor.key == From(Mages) where (_.last.name == "Neera") select (_.last.id))
        > val res1 :Update[Any, Mages, Int] = ...
        
        //An update with explicit unbound parameters:
        > Update(Mages)[Familiar]["name", String].set(_.level += 1, _.familiar := _[Familiar]) where (_.name == _("name"))
        > val res2 :Update[(Familiar, String), Mages, Int] = ...
        
        //An explicit update using an unbound parameter Mage:
        > Update(Mages) set Seq(_.level := _(_.level), _.familiar := _(_.familiar)) where (_.name == _(_.name))
        > val res3 :Update[Mage, Mages, Int] = ...
        
        //Default, late binding update of a row based on its primary key:
        > Update one Mages //set, supplant, where clauses optional
        > val res4 :Update[Mage, Mages, Int] = ...
        
        //A JDBC batch of 'Update one Mages'
        > Update many Mages //set, where, and returning clauses optional as before
        > val res5 :Updates[Iterable[Mage], Mages, Int]] = ...

  - Insert:
    
        //An explicitly written insert using bound parameters:
        > Insert into Mages set Seq(_.level :=? 20, _.name :=? "Morrigan", _.familiar := Familiar("Eyes", "spider"))
        > val res0 :Insert[Any, Mages, Int] = ...
    
        > Insert(gandalf) into Mages supplant(_.level := 40.?) returning (_.id)
        > val res1 :Insert[Any, Mages, Int]
    
        > Insert(Mages)["level", Int]["name", String][Familiar].set( \\
            _.level := _("level"), _.name := _("name"), _.familiar := _[Familiar])
        > val res2 :insert[(Int, String, Familiar), Mages, Int] = ...
    
        //A traditional INSERT ... VALUES with explicit parameters:
        > Insert into Mages values (_.level) x (_.name) x (_.familiar)
        > val res3 :Insert[(Int, String, Familiar), Mages, Int]
    
        //An insert returning the argument Mage updated with auto generated keys
        > Insert returning Mages
        > val res4 :Insert[Mage, Mages, Mage] = ...
    
        > Insert into ListedSpells select ( \\
            From(Mages) as "pupil" join (_.tutor) join (_.spellbook) join ListedSpells as "spells" on (_.id == _.spellbook.key) \\ 
                select (_("pupil").id.ref, _("spells").spell) \\  
            ) \\
        )
        > val res5 :Insert[Any, Mages, Int]

Many other combinations are also possible.


### Other features
Any column or component of a meta-model can be marked as optional or excluded from 
_insert_, _update_, _select_, _compare_. For example, a primary key column is never updated,
may potentially be inserted, depending on a strategy, and almost certainly needs to be selected.
On the other hand, large columns like `BLOB` may be better included in a _select_ only when needed,
but cannot be compared with each other. Any table can be projected to a subset of its columns by
including/excluding certain subcomponents. For example assuming the `Mages.familiar` component is marked
with `OptionalSelect` and has a subject of `Option[Familiar]` rather than `Familiar`,

    From(Mages(-_.familiar)) select *

would not include columns `familiar_name`, `familiar_species` in the _select_ clause of the generated SQL.


### Under the hood
OldSQL is written in a pure object-oriented fashion, as a set of highly polymorphic components.
It is designed with extensibility in mind, and almost any functionality can be swapped out 
for an alternative or custom implementation, with override 'hot spots' on multiple levels.
The design was guided by the principles of *programmer is always right* and *bug oriented development* (tm).

#### You can shoot yourself in the foot if you want
The application developer, as the user is in the best position to tell what constitutes valid code.
The framework tries its best to make making common mistakes hard, but it gives priority to ensuring 
that valid SQL constructs are allowed, even if it would mean that some erroneous code could potentially compile.
A lot of type safety is achieved by type features which are erased in the runtime, and member types and match types
are chosen over implicit evidence wherever possible. As a result, while operations are generally very strictly typed,
it is usually possible to circumvent at least partially the restrictions. SQL expressions are invariant in their 
value type to reduce the chances of type checking expressions incompatible on SQL level, but basically any 
value conversion can be applied to make interoperability easier if expressions are used only in covariant positions,
and so on.

#### 'There is always one more bug'
Therefore, rather than pretend our testing caught everything, and that we have foreseen all possible use cases,
a framework should make it possible to fix bugs and address limitations by plugging in a custom component
in place of a default one, without a need to refactor actual application code.
