# OldSQL
_(old school)_
#### A Scala ORM for programmers not afraid of SQL.

## Philosophy

OldSQL is an object-relational mapper for Scala based around principles of explicit statement of intent
and transparent, modular architecture.

OldSQL implements a certain paradigm shift in thinking about relational database access. 
Historically, most object-relational mappers featured a black box design, doing most work behind the scenes 
and exposing a relatively small API. It is most evident in the _Java_ ecosystem, with _JPA_ and its _Hibernate_ roots. 
The level of complexity of the task meant that a hands-on approach would be much too verbose to be practical. 
As the result, traditional solutions rely on reflection, runtime code generation and favour a declarative approach 
to configuration of mappings between application classes and database tables. Based on imperative, 
object-oriented programming paradigm, they provide functionality of lazy loading and automatic persistence. 
The cost of all the magic is a monolithic design where all functionality is deeply baked in, 
and adherence to the 'there is one way to do it' principle.

Over twenty years of experience resulted in verification of certain ideas, and changes in what is considered 
best practices: from request-bound lifecycle of database sessions and transactions spanning multiple operations, 
to a transaction-per-statement approach keeping database connections open for as short as possible. Arguably, 
this evolution left some features vestigial, while others proved to cause new problems in place of those they
were trying to solve.

Enter the stage _Scala_, a very flexible language, with a rich type system and an open syntax, yielding itself
very well to definitions of _domain-specific languages_. What once was unfeasible, is now possible to express concisely,
bringing (almost) full power to programmers' fingertips.

### Principles

  - WYSIWYG (_what you see is what you get_) approach to the design. Instead of transparent behaviours 
    relying on magic, _OldSQL_ brings explicit commands, and data model design aware of the fact that related entities,
    or even parts of a loaded entity, may not be present on any given object. Functionality like lazy loading is gone
    (by default at least), replaced with explicit yet concise commands. Already the domain model-level API makes
    the behaviour behind it much more evident, and SQL DSL translates to exactly what you want.
  - A programmatic, rather than declarative API. Restrictive configuration files and annotations, serving
    as a front-end to fixed functionalities and resulting in a "there is one way to do it" data mappings, 
    are replaced with standard object creation and method calls wherever possible, bringing the benefits of type safety,
    a gentler learning curve, and a natural process of feature discovery by following method signatures and interface
    implementations from an IDE.
  - An open architecture. _OldSQL_ is not a service with flat API, but a framework and a library. Just as _Scala_
    avoids introducing fixed language features and favours providing general mechanisms available to all programmers, 
    such as string interpolation, arbitrary method names, or right-associative methods, most OldSQL functionality is
    implemented using the API available to programmers: for example, there is no library-spanning support for
    relationship loading, but components implementing everything needed to map collection types, 
    and a public interface which can be used to introduce custom aggregate types such as matrices. 
  - _The programmer is always right._ Magic is great as long as you are not very particular 
    about how the things are done, and savings from much simplified common use cases can be squandered on
    debugging more complex problems, and attempts to force the tool to do exactly what you want. Libraries preventing 
    a valid use in a misguided attempt to save the users' from themselves are among the most frustrating situations
    in this job. _OldSQL_ promotes a certain style, but it also tries to always leave the way open for the programmers 
    to "switch to the manual mode" and override any functionality, by either resorting to lower-level interfaces,
    introducing new type classes for their types, or by plugging in custom component implementations. It relies heavily
    on implicit parameters and macros for some "quality of life" features, but, in general, favours member types 
    and object-oriented polymorphism in its implementation, and, with a bit of persistence in casting, all core 
    functionality can be used in a type-unsafe manner if the situation demands it.
  - The programmer must know SQL. While "Hello world" problems almost always look beautifully simple, ultimately
    efficiency demands that the programmer understands exactly what their high-level operations translate to.
    A custom query language or API will always be less expressive and worse documented than SQL; it is yet another thing
    requiring getting accustomed to, and can often annoyingly stand in the way of getting the results you want.
  - An immutable data model. Scala promotes functional programming style and _OldSQL_ follows suit by assuming
    that mapped classes are _effectively_ immutable. It considerably simplifies certain areas, for example
    by getting rid of persistence state tracking of all loaded entities and collections, while making 
    heavier optimizations possible in others.
  - Component oriented. There are very few integers and strings in the real world. Humans do not have street names 
    and phone numbers are not simply text. We postulate that almost all standard types would be better wrapped 
    in simple classes, and entities should not be flat _one-to-one_ mappings of the underlying tables, 
    but instead follow the principles of good object-oriented design. Introducing mappings for custom, 
    reusable value types, should be as easy as possible and a natural habit.
  - Ideally, any sensible schema for a given business model should be mappable to any well-designed, object-oriented 
    set of classes realising the same model. While not attainable.


## Features