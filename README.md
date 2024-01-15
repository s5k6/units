
  * Are there units with fractional exponents?

  * Looking at HM: The types inferred for a term seem to have less
    information than the term: Different terms can have the same type.
    Looking at quantities, I suspect that inference of the unit of a
    term pretty much directly reflects the term's structure.  This
    exposes all information at value level to the type level.  I
    cannot yet put my finger on it right now, but this feels like
    making types involving units less useful.  Why exactly?

  * If the units of a function were known a priori, then they
    could/should be calculated/checked at compile time, and only the
    base values should be calculated at runtime.  This sounds like
    unboxing/boxing at compile time.  Need to investigate how this
    could be done.  Is this the Church vs. Curry question for units?
