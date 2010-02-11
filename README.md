<head>
 <title>ASDF.eXperimental</title>
</head>

ASDF.X: an experimental feature-equivalent direct substitute for another system definition facility
-------

The central task of [ASDF](http://common-lisp.net/project/asdf/getting-started.htm) is to 
propagate existential and temporal constraints within a set of program components.
In particular, it acts on the temporal properties of load and compile-file events
with repect to lisp source files. 
This version is an experimental deconstruction to isolate the core functions
and reimplement them. It aims to

*   facilitate maintenance and further developement by expressing the terms for
the interpretation in the model fields and exposing the logic in the operator interfaces
*   provide a layered definition mechanism
    *   a well-defined macro layer; see [defsystem](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L512),
defcomponent, defmodule, defelement
    *   a well-defined set of definition operators; see [define-system](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2472),
define-component
    *   component designators which permit inter-module references as well as the current
intra-module and top-level inter-system; see [find-component](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L1456)
*   model system definition and construction in terms of a static annotated graph
    *   canonical forms for constituent and requirement dependency expressions; see
[canonicalize-component-option](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2874)
and [canonicalize-requirement.](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L1300),
which attempt a balance between a standard representation and ease of expression should one
use the function interface
    *   an operate/perform interpreter which applies the annotations to its
dynamic state as it traverses the dependency graph; see [operate](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L1640),
[perform.dependency](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2001), 
[perform-constituent](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2144),
and [perform-requirement.](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2112)
    *   performance operators which report completion state and permit the interpreter
to apply warn/fail/ingore rules as per annotation; see [compute-performance-status](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2180),
perform([compile-op](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2300)),
([load-op](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2352),
&co
    *   explicit representation and interpretation of self/constituent/requirement
traversal-order rules; see perform.dependency.
*   a mechanism which permits to use the package `asdf-user` as the default to load
system definition files; see [load-system-definition](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2359).
*   a mechanism which locates and loads component and operator class definitions on-demand
in the same manner as system definition files; see [context-find-class](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2498).
*   explicit declarations for the implicit reflexive and transitive requirment relations
like "compile before loading" and "load requirements before compiling"; see canonicalize-requirement,
[reflexive-operations](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L951),
and [transitive-operations](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L956)
*   reformulate the method combination with `restart` and `dependency` qualifiers to account
for their purpose, and factor the respective behaviour into the component class model; see
[standard-asdf-method-combination](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L548),
[restartable-component](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L970),
[perform.restart](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L1964),
[perform.dependency](http://github.com/lisp/de.setf.asdf.x/blob/518eb571e0c8a13d95ee2737c681cec5379be51e/asdf-x.lisp#L2001)


The implementation intends plug-compatibility with ASDF as of +/- version 1.5,
with one significant distinction. Given that it compiles a complete graph
from the system definitions, and traverses that static graph to perform
operations contingent on its dynamic state and the results of respective
`(operator x component)` combinations, the graph walk does not follow the same
path as the original. This means that, for example

*   nodes are examined as many times as they appear in the graph and subject to
re-performance if their state changes or the environment changes
*   featurea are interpreted dynamically rather than at the outset.

The core of the implementation is the `perform` operator - in particular the `dependency`
method:

  operate (operator component)
  -> perform (operator component)
     -> perform-constituent (op-self self required-op required-component)
        -> component-constituency-requirements
        perform-requirement (op-self self required-op required-component)
        -> component-dependency-requirements

This subsumes the current `traverse` operator. It acts as follows

*   establish a standard processor state for each (operator x component) invocation
*   assert any settings declared for the combination
*   peforms the local operation and/or propagates local and transitive operations according to
the current traversal setting. at the present reflexive requirements appear at the head of
transitive ones, but it would be better to model them as their own branch set. each branch is
expressed in the form
    (this-node (this-operation . requirements))
where the requirements take one of two forms: propagation and local state change.
propagation is expressed as
    (remote-operation target-node)
local state change is expressed as
    (:setting value)
The setting side-effects serve, for example, to express weak and contingent dependencies as
changes to  failure- or missing-behaviour rather than graph modifications.
*   interprets performance results in the context of local settings to warn, error, or proceed.


Status
------

The `asdf.lisp` contains the core functionality only. Other facilities have been factored
out to respective files for safe-keeping.

*   `asdf-configure.lisp` : the configuration facility
*   `asdf-sbcl.lisp` : sbcl-specific code
*   `asdf-pathname-resolver.lisp` : the aol/abl facility

The code is projective, but has built in both mcl-5.2 and sbcl-1.0.35, and is
operable to the extent that it passed `test/dweinreb-tests.lisp` with modifications for
the changed model interpretation semantics. While one always intends to get things right, the
present stage make minimal claims to correctness. The intent is to provide a platform
on which to understand how this build system works and how one might work.
For example, two immediate questions are

*    which constraints are to propagate between which components in which directions
*    should the peform.dependency method be combined like normal methods, or should it be
somehow specific to the class. either delegate to an additional operator to recognize
an independent combination, or combined such that only the most specific is included, or...?

It defines and resides in the package `asdf.x`, to which, iff no `asdf` package is present, that name
is added as a nickname. As such, it should coexist with an exisiting ASDF for examination
purposes.


