
Calculations with physical quantities
=====================================

Speed after falling for 10s under 1g:

    $ units
    9.81 [m/s2] * 10 [s] * 3600[s/h] / 1000[m/km]
    = 353.1600000000001 [km / h]

Only the trivial cases are trivial.  A playground for thoughts.


Compile
-------

    $ env -C dep ./get-dependencies
    $ stack build
    $ stack exec "$SHELL"

The last command yields a shell with `units` on the `$PATH`.


Run
---

Program `units` reads one line from stdin, containing an arithmetic
expression over quantities.  A quantity is a floating point number
with an optional unit.  Syntax for units is experimental.

Examples:

    $ ./demo

Interactive:

    $ while rlwrap units; do echo; done

The units are only words without meaning.  Relating them, and
reconstructing composite units (e.g. Volts) will require a database of
derived units, and a means to choose which base units should be
combined for output.


Thoughts
--------

  * There's probably research on this.  Find it.

  * Are there units with fractional exponents?  The only one I've
    found is “noise amplitude spectral density” [1], given in `V /
    Hz^(1/2)` — volts per root hertz.  Is this relevant?  Currently
    unsupported.

  * If ℕ is not enough, are there units with exponents outside of ℚ?

  * Looking at Hindley-Milner, the types inferred for a term have less
    information than the term: Different terms can have the same type
    (which allows for different implementations to satisfy a required
    term).

    Looking at quantities, I suspect that inference of the unit of a
    term pretty much directly reflects the term's structure.  This
    would expose all information at value level to the type level.

    I cannot put my finger on it right now, but this feels like making
    types involving units less useful.  Why exactly?

  * If the units were at the type level, then exponentiation would
    yield dependent types:

        1 [m] ^ (1 + 2)

    The resulting unit depends on the value of the expression `1 + 2`,
    which cannot be known to a static type checker.  Need to research
    dependent typing.

    One could chicken out by allowing only constant exponents, quite
    harsh though.  In a more powerful language, one with loops, one
    might be tempted to re-implement non-constant exponentiation,
    raising the problem again.

  * For cases where the unit does not depend on a value (i.e., no
    exponentiation), it is fixed at compile time.  Then runtime
    calculation is redundant.  Is there a way to tell these cases
    apart?

  * How do units beave across more complex functions?  I.e., when
    quantities are argument to `div`, `sin`, `exp`, `ln`, `sqrt`, …?

  * How exactly does the boxing/unboxing concept, with type
    constructors tailored to the physical dimension(s), differ from
    automatic calculation of units?  For one, naively done (un)boxing
    either involves nesting (implying order) or a different type for
    every combination of dimensions.  The algebra of arithmetics used
    in the traditional calculation of units compensates for this.


Syntax ideas
------------

Different styles are possible.  Which one is convenient for writing?
How well does it play with variables and function application?

   2 N·m/s²     — conflict with division, difficult typing exponent

   2 [N m/s2]   — implemented

   2 N m|s2     — attribution less clear

FIXME elaborate


[1]: https://en.wikipedia.org/wiki/Noise_spectral_density
