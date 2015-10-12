This is copied from some architectural notes on paper dated 2015-08-28.
As such, it might be a bit outdated or totally confused.  This is here
to explain the thought with some of the design decisions.

1. The current Ion implementation is very close to coroutines.  In
some sense, it's already a continuation.  It "suspends" and
"resumes" - but only on clock ticks right now.
2. It looks feasible to force local state to be in the form of globals
that (Haskell-wise) are local to the monad.
3. To let me compose multiple Ions, multiple suspend and resume points
must coexist.
4. Globals will require some kind of prefix to not clash.  (This
applies really whether I do coroutines or not, as it applies
anytime one instantiates a spec twice.)
5. I can't see a meaningful way I should let Ion express _sequential_
code inside a coroutine.  I see only Ivory code here.  With this I
can enforce the atomicity/synchronicity as in Atom, though I may
need to 'lift' up variable access.
6. At no point must 'suspend' be handled separately.  It suspends at
the end of flow in Ion anyway.  The change is only in defining how
it changes.
7. We 'reify' that resume point into a continuation in the form of a
function pointer by bundling a state that encodes position inside
the spec where one resumes.
8. No sensible way appears to exist of letting Ion scheduling happen
_inside_ a coroutine.  The Ion monad in general has no place inside
a coroutine - the schedule's meaningless (we can't assign a phase
at compile-time), and the components aren't sequential; Ivory's
needed for most things.
9. I might be able to move values around with a continuation still.
10. Some Ivory ponderances:

- Have ivoryEff return values; perhaps write to MemAreas.
- Lift accesses to internal MemAreas to enforce Atom's stuff?
- How could I do exceptions with this?

11. Composition means that I must be able to call another Ion somehow.
I'm not sure how yet.  If I do so then it must convey its own
return continuation for whatever resume point.  (In the margins:
Not exactly.)
12. An important question: How would I get the continuation outside of
that local scope so that I could schedule calls to it within Ion?
13. Proposed code is like:

> void ion_ ... (uint16_t state) {
>     if (state == 0) {
>        ... usual dispatch ...
>     } else if (state == 1) {
>        ... call resume at some point ...
>     } else if (state == 2) {
>        ... likewise ...
>     }
> }

14. Could I meaningfully decouple the continuation stuff from the
scheduling code?  I still struggle, despite reasons around #1, #2, #10
to find a reason that coroutine/continuation needs to reside
in Ion.
15. Any approach that wants to pass a function pointer to an Ivory
procedure at C runtime, and have that function pointed called, or even
used at all, after that procedure has returned, will have to deal with
the fact that one can't put a ProcPtr in a MemArea in Ivory.
16. In what sense is the simple function call I generate a
"continuation"?  Is it only in the sense that I am using it to build
things up in CPS?  The key in Ivory's coroutine implementation is the
abstraction it provides and the ability to clearly say 'suspend here,
and resume here' while still behaving like a 'normal' Ivory procedure.
So, perhaps what I have is nothing like coroutines, just an
enabler/helper for CPS that can be async.
17. The entire mechanism of 'delay' is a bit ambiguous.  For instance,
what is the phase at X here? 30, 50?  Both anwsers and ugly and
annoying in some way.  In the software now, it's just 20 if the whole
thing runs in phase 0.

> phase 10 $
>    ...
>    phase 20 $
>       ...
> delay 20 $ X

18. You can sort of fix 17 by having delay work only with the phase
that is passed to it, e.g.

> phase 10 $
>    delay 20 $ ...  -- phase 30 here
>    delay 30 $ ...  -- phase 40 here
>    delay 40 $ do
>       ...          -- phase 50 here
>       delay 50 $ ...  -- phase 100 there

This is explicit and it works.  The problems with it are that it
trades off nice monadic expression with nested function composition,
and that it nests deeper than otherwise needed, which can make for
very lengthy node paths.
