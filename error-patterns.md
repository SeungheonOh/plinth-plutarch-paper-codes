# Error Injection Patterns for Compiler Error Message Evaluation

This document defines a set of common programming mistakes to be injected into
both Plinth and Plutarch validators for evaluating compiler error message quality.
Each pattern represents the same logical mistake expressed in both languages,
producing paired error messages for side-by-side comparison.

Patterns are drawn from three empirical studies of beginner Haskell errors:

- **Tirronen15**: Tirronen, V., Uusi-Mäkelä, S., & Isomöttönen, V. (2015).
  *Understanding beginners' mistakes with Haskell.* JFP 25, e11.
- **Singer18**: Singer, J. & Archibald, B. (2018).
  *Functional Baby Talk: Analysis of Code Fragments from Novice Haskell Programmers.*
  TFPIE 2017, EPTCS 270.
- **Nemeth19**: Németh, Choi, Makihara & Iida (2019).
  *Investigating Compilation Errors of Students Learning Haskell.*
  TFPIE 2018, EPTCS 295.

Each pattern is scored on two axes (1–5 scale):

- **Jargon level**: Does the error message use terms from the source language (5),
  or does it expose compiler internals, type-family reductions, or Core types (1)?
- **Location precision**: Does the message point to the exact line of the bug (5),
  or to an unrelated location / no location at all (1)?

---

## Category 1: Type Mismatches

### E01 — Return list instead of element

**Root cause**: Container-vs-element confusion
(Tirronen15 §CouldntMatch cause 1; Nemeth19 §list-type-mismatch)

In the accumulator position of a fold, return a singleton list `[x]` where a bare
value `x` is expected.

**Plinth injection**:
```haskell
-- In sumMaybeIntegers, change accumulator update:
--   then acc + n
-- to:
    then [acc + n]
```

**Plutarch injection**:
```haskell
-- In psumEvenMaybeIntegers, change accumulator update:
--   (acc + pfromData n)
-- to:
    pcon (PCons (acc + pfromData n) (pcon PNil))
```

### E02 — Swap two arguments

**Root cause**: Genuine type mismatch from argument reordering
(Tirronen15 §CouldntMatch cause 1)

Pass arguments to a two-argument function in the wrong order (e.g., swap the
accumulator and the list in a fold).

**Plinth injection**:
```haskell
-- Change: foldList (\acc mx -> ...) 0
-- to:     foldList 0 (\acc mx -> ...)
```

**Plutarch injection**:
```haskell
-- Change: pfoldPList # plam (\acc mx -> ...) # 0
-- to:     pfoldPList # 0 # plam (\acc mx -> ...)
```

### E03 — Use element where list expected

**Root cause**: Container-vs-element confusion, inverse of E01
(Tirronen15 §CouldntMatch cause 1; Nemeth19 §list-type-mismatch)

Pass a single value where a list type is expected.

**Plinth injection**:
```haskell
-- Where a List is expected, pass a bare Integer:
-- Change: sumMaybeIntegers myList
-- to:     sumMaybeIntegers 42
```

**Plutarch injection**:
```haskell
-- Where PList is expected, pass a bare PInteger term:
-- Change: psumEvenMaybeIntegers # myList
-- to:     psumEvenMaybeIntegers # (pconstant 42)
```

### E04 — Numeric division on integers

**Root cause**: Numeric-type confusion; using fractional division on integral type
(Tirronen15 §NoInstance cause 2 "WrongNum"; Nemeth19 §simple-type-mismatch)

Use `/` on `Integer` / `PInteger` instead of `divide` / `pdiv`.

**Plinth injection**:
```haskell
-- Change: n `divide` 2
-- to:     n / 2
```

**Plutarch injection**:
```haskell
-- Change: pdiv # n' # 2
-- to:     n' / 2
```

### E05 — Boolean where integer expected (or vice versa)

**Root cause**: Simple type mismatch between unrelated base types
(Nemeth19 §simple-type-mismatch; Singer18 §type-errors "No instance for (Num Bool)")

Return a boolean in a branch that should return an integer.

**Plinth injection**:
```haskell
-- In netWithdraw, change: Amount n -> n
-- to:                      Amount n -> True
```

**Plutarch injection**:
```haskell
-- In netWithdraw, change: PAmount n -> pfromData n
-- to:                      PAmount n -> pcon PTrue
```

### E06 — Apply a non-function

**Root cause**: Treating a value as if it were callable
(Tirronen15 §CouldntMatch cause 3 "syntax misunderstanding")

Write a value-level application where the "function" is actually an integer or
other non-function type.

**Plinth injection**:
```haskell
-- Change: netWithdraw from - n
-- to:     netWithdraw from - n x    (where x is a bound variable)
```

**Plutarch injection**:
```haskell
-- Change: (self # pfromData from) - pfromData n
-- to:     (self # pfromData from) - (pfromData n # x)
```

### E07 — Wrong field from a product

**Root cause**: Accessing the wrong field of a multi-field constructor
(Tirronen15 §CouldntMatch cause 1)

In a constructor with multiple fields of different types, use a field at the type
of a sibling field.

**Plinth injection**:
```haskell
-- In Deduct n from, change: netWithdraw from - n
-- to:                        netWithdraw n - from
-- (n :: Integer used where Withdraw expected, and vice versa)
```

**Plutarch injection**:
```haskell
-- In PDeduct n from, change: (self # pfromData from) - pfromData n
-- to:                         (self # pfromData n) - pfromData from
-- (n :: PAsData PInteger used where PAsData PWithdraw expected)
```

---

## Category 2: Missing Constraints

### E08 — Missing equality constraint

**Root cause**: Using an ad-hoc polymorphic function without the required class constraint
(Tirronen15 §NoInstance cause 3 "missing constraint")

Compare two values of a polymorphic type without the `Eq` / `PEq` constraint.

**Plinth injection**:
```haskell
-- Add a helper with missing constraint:
contains :: a -> [a] -> Bool        -- should be: (Eq a) =>
contains x (y:ys) = x == y || contains x ys
```

**Plutarch injection**:
```haskell
-- Add a helper with missing constraint:
pcontains :: Term s (a :--> PList a :--> PBool)   -- should be: (PEq a) =>
-- ... uses #== inside
```

### E09 — Missing ordering constraint

**Root cause**: Same as E08 but for ordered comparison
(Tirronen15 §NoInstance)

Use `<` / `<=` without `Ord` / `POrd` on a polymorphic type.

**Plinth injection**:
```haskell
clamp :: a -> a -> a -> a            -- should be: (Ord a) =>
clamp lo hi x = if x < lo then lo else if x > hi then hi else x
```

**Plutarch injection**:
```haskell
pclamp :: Term s (a :--> a :--> a :--> a)   -- should be: (POrd a) =>
-- ... uses #< inside
```

### E10 — Missing Data-conversion constraint

**Root cause**: Missing class constraint for Data encoding/decoding
(Tirronen15 §NoInstance; domain-specific)

Operate on a polymorphic type that needs Data conversion but omit the
`ToData`/`UnsafeFromData` or `PIsData` constraint.

**Plinth injection**:
```haskell
-- Change: fromMaybeData :: (ToData a, UnsafeFromData a) => a -> MaybeData a -> a
-- to:     fromMaybeData :: a -> MaybeData a -> a
```

**Plutarch injection**:
```haskell
-- Change: pfromMaybeData :: (PIsData a) => Term s (a :--> PMaybeData a :--> a)
-- to:     pfromMaybeData :: Term s (a :--> PMaybeData a :--> a)
```

---

## Category 3: Precedence and Application

### E11 — Missing parentheses around constructor pattern

**Root cause**: Omitting parentheses in pattern match on constructor
(Tirronen15 §ParseError cause 2)

Write `f x:xs = ...` instead of `f (x:xs) = ...`. In Plutarch, the equivalent
occurs inside the body of a `pmatch` case when destructuring a sub-expression.

**Plinth injection**:
```haskell
-- Change: foldList f acc (Cons x rest) = ...
-- to:     foldList f acc Cons x rest = ...
```

**Plutarch injection**:
```haskell
-- In a helper that manually destructures:
-- Change: \(PCons x rest) -> ...
-- to:     \PCons x rest -> ...
```

### E12 — Missing parentheses in function chain

**Root cause**: Precedence confusion in multi-argument application
(Tirronen15 §CouldntMatch cause 2 "precedence";
 Nemeth19 §wrongly-grouped-parameters)

Write `f g x` meaning `f (g x)` — the compiler parses it as `(f g) x`.

**Plinth injection**:
```haskell
-- Change: netWithdraw (Deduct n from)
-- to:     netWithdraw Deduct n from
```

**Plutarch injection**:
```haskell
-- Change: self # pfromData from
-- to:     self # pfromData # from
-- (applies pfromData to self, then result to from)
```

### E13 — Operator section confusion

**Root cause**: Confusing prefix and infix usage
(Nemeth19 §confusing-name-and-operator-usage)

Use an operator in prefix position without parentheses, or a named function
in infix position without backticks.

**Plinth injection**:
```haskell
-- Change: n `modulo` 2
-- to:     modulo n 2    -- this actually works, so instead:
--         n modulo 2    -- missing backticks
```

**Plutarch injection**:
```haskell
-- Change: pmod # n' # 2
-- to:     n' pmod 2     -- trying to use pmod as infix without backticks
```

---

## Category 4: Naming and Scope

### E14 — Typo in function name

**Root cause**: Misspelling a commonly used identifier
(Tirronen15 §NotInScope cause 1 "typos";
 Singer18 §not-in-scope "lenght, putsStrLn";
 Nemeth19 §definition-name-mistake)

Misspell a function name that both languages provide equivalents of.

**Plinth injection**:
```haskell
-- Change: modulo
-- to:     modul
```

**Plutarch injection**:
```haskell
-- Change: pfromData
-- to:     pfromDat
```

### E15 — Lowercase constructor

**Root cause**: Case error on data constructors
(Tirronen15 §NotInScope cause 2 "faulty syntax";
 Nemeth19 §global-name-mistake)

Write a constructor name in lowercase.

**Plinth injection**:
```haskell
-- Change: DNothing
-- to:     dNothing
```

**Plutarch injection**:
```haskell
-- Change: PDNothing
-- to:     pDNothing
```

### E16 — Variable not in scope (wrong binding)

**Root cause**: Referencing a variable that doesn't exist in the current scope
(Tirronen15 §NotInScope cause 4 "scope";
 Nemeth19 §local-name-mistake)

Reference an intermediate binding from a sibling equation or a different branch.

**Plinth injection**:
```haskell
-- In netWithdraw:
--   Amount n -> n
--   Joint x y -> netWithdraw x + netWithdraw y
--   Deduct n from -> netWithdraw from - amount
--                                       ^^^^^^ not in scope (meant: n)
```

**Plutarch injection**:
```haskell
-- In netWithdraw pmatch:
--   PAmount n -> pfromData n
--   PJoint x y -> ...
--   PDeduct n from -> (self # pfromData from) - pfromData amount
--                                                        ^^^^^^ not in scope
```

---

## Category 5: Pattern Matching and Exhaustiveness

### E17 — Missing base case

**Root cause**: Omitting the base case of a recursive function
(Tirronen15 §NonExhaustive — "most commonly when the student has forgotten
to include a base case for a recursive function")

Omit the nil/empty case in a list fold.

**Plinth injection**:
```haskell
-- Remove: foldList _ acc Nil = acc
-- Keep only the recursive case
```

**Plutarch injection**:
```haskell
-- In pfoldPList, remove: PNil -> acc
-- Keep only the PCons case
```

### E18 — Missing constructor in multi-constructor match

**Root cause**: Forgetting one constructor of a 3+ constructor type
(Tirronen15 §NonExhaustive)

In the `Withdraw`/`PWithdraw` type (3 constructors: Amount, Joint, Deduct),
handle two but omit the third.

**Plinth injection**:
```haskell
-- Remove the Deduct case from netWithdraw
netWithdraw w = case w of
  Amount n -> n
  Joint x y -> netWithdraw x + netWithdraw y
  -- Deduct case missing
```

**Plutarch injection**:
```haskell
-- Remove the PDeduct case from netWithdraw pmatch
case w of
  PAmount n -> pfromData n
  PJoint x y -> ...
  -- PDeduct case missing
```

### E19 — Wrong number of fields in pattern

**Root cause**: Constructor arity mismatch
(Tirronen15 §ArgsNum)

Pattern match a two-field constructor with one binding.

**Plinth injection**:
```haskell
-- Change: Joint x y -> netWithdraw x + netWithdraw y
-- to:     Joint x -> netWithdraw x
```

**Plutarch injection**:
```haskell
-- Change: PJoint x y -> ...
-- to:     PJoint x -> ...
```

---

## Category 6: Wrong Function / Wrong Operation

### E20 — Confuse list concatenation functions

**Root cause**: Using a similarly-named function with the wrong type
(Nemeth19 §wrong-operation-applied — "used concat instead of ++")

Use a function that operates on nested lists where flat-list append is intended,
or vice versa.

**Plinth injection**:
```haskell
-- Where (++) is appropriate:
-- Change: xs ++ ys
-- to:     concat [xs, ys]    -- works, but change to: concat xs ys (wrong arity)
```

**Plutarch injection**:
```haskell
-- Analogous: use a nested-list combinator where a flat append is needed
```

### E21 — Use wrong Prelude function on custom type

**Root cause**: Applying a standard library function to a user-defined type
(Nemeth19 §wrong-operation-applied)

Call Haskell's `head` on a custom `List` type / Plutarch's `phead` on `PList`
when the types don't align with the built-in list type.

**Plinth injection**:
```haskell
-- Change: case myList of Cons x _ -> x
-- to:     head myList       -- head expects [a], not List a
```

**Plutarch injection**:
```haskell
-- Change: pmatch myList $ \case PCons x _ -> x
-- to:     phead # myList    -- phead expects PBuiltinList, not PList
```

### E22 — Equality test on function type

**Root cause**: Attempting to compare non-comparable types
(Tirronen15 §NoInstance)

Accidentally try to compare two function-typed values for equality.

**Plinth injection**:
```haskell
-- Write: if netWithdraw == netWithdraw then ...
```

**Plutarch injection**:
```haskell
-- Write: pif (netWithdraw #== netWithdraw) ...
```

---

## Category 7: Recursion

### E23 — Wrong pattern order (general before specific)

**Root cause**: Catch-all pattern shadowing base case
(Tirronen15 §StackOverflow — "wrong order of patterns where a general
capturing pattern shadows the base case")

Put a wildcard or general pattern before the base case so the base case
is unreachable.

**Plinth injection**:
```haskell
-- Reorder:
foldList f acc (Cons x rest) = foldList f (f acc x) rest
foldList _ acc Nil = acc           -- unreachable
```

**Plutarch injection**:
```haskell
-- Reorder cases in pmatch handler:
PCons x rest -> self # f # (f # acc # x) # rest
PNil -> acc                        -- unreachable
```

### E24 — Self-reference without proper recursion setup

**Root cause**: Defining a recursive function without making it actually recursive
(Tirronen15 §StackOverflow)

In Plinth, use a lambda instead of a named equation so the function can't
refer to itself. In Plutarch, thread `self` from `pfix` but call the outer
name instead.

**Plinth injection**:
```haskell
-- Change named recursion to lambda without self-reference:
-- Change: netWithdraw w = case w of
--           Joint x y -> netWithdraw x + netWithdraw y
-- to:     netWithdraw = \w -> case w of
--           Joint x y -> netWithdraw x + netWithdraw y
-- (This actually still works in Haskell because let-bindings are recursive.
--  Better injection: accidentally shadow the name)
--           Joint x y -> let netWithdraw = undefined in netWithdraw x + netWithdraw y
```

**Plutarch injection**:
```haskell
-- Forget to use self, call the top-level binding instead:
-- Change: pfix #$ plam $ \self wt -> ... self # pfromData x ...
-- to:     pfix #$ plam $ \self wt -> ... netWithdraw # pfromData x ...
-- (references the compiled Term, not the recursive self)
```

---

## Category 8: Type System Edge Cases

### E25 — Infinite type (occurs check)

**Root cause**: Accidentally creating a self-referential type equation
(Tirronen15 §InfiniteType — "confusing a container with one of its elements")

Confuse the list itself with one of its elements in a definition.

**Plinth injection**:
```haskell
-- Change: Cons x rest -> foldList f (f acc x) rest
-- to:     Cons x rest -> foldList f (f acc rest) x
-- (rest :: List a used as element, x :: a used as List a)
```

**Plutarch injection**:
```haskell
-- Change: PCons x rest -> self # f # (f # acc # x) # rest
-- to:     PCons x rest -> self # f # (f # acc # rest) # x
```

### E26 — Ambiguous type variable

**Root cause**: Polymorphic expression where the compiler cannot infer the type
(Tirronen15 §NoInstance)

Write a round-trip through a conversion (encode then decode) without pinning
the intermediate type, so the compiler doesn't know which instance to use.

**Plinth injection**:
```haskell
-- Change: fromBuiltinData (toBuiltinData x)   -- x has known type
-- to:     fromBuiltinData (toBuiltinData (fromBuiltinData d))
--         where the intermediate type is ambiguous
```

**Plutarch injection**:
```haskell
-- Change: pfromData (pdata x)   -- x has known type
-- to:     pfromData (pdata (pfromData d))
--         where the intermediate type is ambiguous
```

---

## Evaluation Procedure

For each pattern E01–E26:

1. Inject the bug into the same validator in both Plinth and Plutarch.
2. Compile and capture the full compiler error output.
3. Score each error message on:
   - **Jargon** (1–5): 5 = message uses only source-level terms;
     1 = message exposes GHC Core, type-family reductions, or internal representations.
   - **Location** (1–5): 5 = error points to the exact line of the injected bug;
     1 = error points to an unrelated location or provides no location.
4. Record both scores and the full error text.

Results are reported in a table with one row per pattern and columns for
Plinth jargon, Plinth location, Plutarch jargon, and Plutarch location.
