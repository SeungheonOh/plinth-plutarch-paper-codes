# Error Message Comparison: Plinth vs Plutarch

Side-by-side compilation errors for 26 injected bug patterns.
Compiled with GHC 9.6.6 via `cabal exec ghc -- -Wall -fforce-recomp -c`.

---

## Type Mismatches

### E01 — Return list instead of element

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:19:35: error: [GHC-83865]
    • Couldn't match expected type ‘Integer’
                  with actual type ‘List Integer’
    • In the expression: Cons (acc + n) Nil
      In the first argument of ‘foldList’, namely
        ‘(\ acc n -> Cons (acc + n) Nil)’
      In the expression: foldList (\ acc n -> Cons (acc + n) Nil) 0
   |
19 | sumIntegers = foldList (\acc n -> Cons (acc + n) Nil) 0
   |                                   ^^^^^^^^^^^^^^^^^^</pre></td>
<td><pre>Plutarch.hs:39:28: error: [GHC-18872]
    • Couldn't match type ‘PList PInteger’ with ‘PInteger’
        arising from a use of ‘plam’
    • In the second argument of ‘(#)’, namely
        ‘plam (\ acc n -> pcon (PCons (acc + n) (pcon PNil)))’
      In the first argument of ‘(#)’, namely
        ‘pfoldList # plam (\ acc n -> pcon (PCons (acc + n) (pcon PNil)))’
      In the expression:
        pfoldList # plam (\ acc n -> pcon (PCons (acc + n) (pcon PNil)))
          # 0
   |
39 | psumIntegers = pfoldList # plam (\acc n -> pcon (PCons (acc + n) (pcon PNil))) # 0
   |                            ^^^^</pre></td>
</tr>
</table>

### E02 — Swap two arguments

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:19:27: error: [GHC-83865]
    • Couldn't match expected type ‘Integer’
                  with actual type ‘a0 -> a0 -> a0’
    • The lambda expression ‘\ acc n -> acc + n’
      has two value arguments,
        but its type ‘Integer’ has none
      In the second argument of ‘foldList’, namely ‘(\ acc n -> acc + n)’
      In the expression: foldList 0 (\ acc n -> acc + n)
   |
19 | sumIntegers = foldList 0 (\acc n -> acc + n)
   |                           ^^^^^^^^^^^^^^^^^</pre></td>
<td><pre>Plutarch.hs:39:32: error: [GHC-83865]
    • Couldn't match type ‘b :--> (b :--> b)’ with ‘PInteger’
      Expected: Term s PInteger
        Actual: Term s (b :--> (b :--> b))
    • In the second argument of ‘(#)’, namely
        ‘plam (\ acc n -> acc + n)’
      In the expression: pfoldList # 0 # plam (\ acc n -> acc + n)
      In an equation for ‘psumIntegers’:
          psumIntegers = pfoldList # 0 # plam (\ acc n -> acc + n)
   |
39 | psumIntegers = pfoldList # 0 # plam (\acc n -> acc + n)
   |                                ^^^^^^^^^^^^^^^^^^^^^^^^</pre></td>
</tr>
</table>

### E03 — Use element where list expected

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:22:22: error: [GHC-39999]
    • No instance for ‘GHC.Num.Num (List Integer)’
        arising from the literal ‘42’
    • In the first argument of ‘sumIntegers’, namely ‘42’
      In the expression: sumIntegers 42
      In an equation for ‘result’: result = sumIntegers 42
   |
22 | result = sumIntegers 42
   |                      ^^</pre></td>
<td><pre>Plutarch.hs:42:26: error: [GHC-83865]
    • Couldn't match type ‘PInteger’ with ‘PList PInteger’
      Expected: Term s (PList PInteger)
        Actual: Term s PInteger
    • In the second argument of ‘(#)’, namely ‘(42 :: Term s PInteger)’
      In the expression: psumIntegers # (42 :: Term s PInteger)
      In an equation for ‘result’:
          result = psumIntegers # (42 :: Term s PInteger)
   |
42 | result = psumIntegers # (42 :: Term s PInteger)
   |                          ^^^^^^^^^^^^^^^^^^^^^</pre></td>
</tr>
</table>

### E04 — Numeric division on integers

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:27:17: error: [GHC-88464]
    Variable not in scope: (/) :: Integer -> t0 -> Integer
   |
27 |   Amount n -> n / 2
   |                 ^</pre></td>
<td><pre>Plutarch.hs:35:30: error: [GHC-39999]
    • No instance for ‘Fractional (Term s PInteger)’
        arising from a use of ‘/’
    • In the expression: pfromData n / 2
      In a case alternative: PAmount n -> pfromData n / 2
      In the expression:
        case w of
          PAmount n -> pfromData n / 2
          PJoint x y -> (self # pfromData x) + (self # pfromData y)
          PDeduct n from -> (self # pfromData from) - pfromData n
   |
35 |     PAmount n -> pfromData n / 2
   |                              ^</pre></td>
</tr>
</table>

### E05 — Boolean where integer expected

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:27:15: error: [GHC-83865]
    • Couldn't match expected type ‘Integer’ with actual type ‘Bool’
    • In the expression: True
      In a case alternative: Amount n -> True
      In the expression:
        case w of
          Amount n -> True
          Joint x y -> netWithdraw x + netWithdraw y
          Deduct n from -> netWithdraw from - n
   |
27 |   Amount n -> True
   |               ^^^^</pre></td>
<td><pre>Plutarch.hs:33:23: error: [GHC-18872]
    • Couldn't match type ‘PBool’ with ‘PInteger’
        arising from a use of ‘plam’
    • In the first argument of ‘($)’, namely ‘plam’
      In the second argument of ‘(#$)’, namely
        ‘plam
           $ \ self wt
               -> pmatch wt
                    $ \ w
                        -> case w of
                             PAmount n -> ...
                             PJoint x y -> ...
                             PDeduct n from -> ...’
      In the expression:
        pfix
          #$
            plam
              $ \ self wt
                  -> pmatch wt
                       $ \ w
                           -> case w of
                                PAmount n -> ...
                                PJoint x y -> ...
                                PDeduct n from -> ...
   |
33 | netWithdraw = pfix #$ plam $ \self wt -> pmatch wt $ \w ->
   |                       ^^^^

Plutarch.hs:36:20: error: [GHC-83865]
    • Couldn't match type ‘PInteger’ with ‘PBool’
      Expected: Term s (PWithdraw :--> PBool)
        Actual: Term s (PWithdraw :--> PInteger)
    • In the first argument of ‘(#)’, namely ‘self’
      In the first argument of ‘(+)’, namely ‘(self # pfromData x)’
      In the expression: (self # pfromData x) + (self # pfromData y)
   |
36 |     PJoint x y -> (self # pfromData x) + (self # pfromData y)
   |                    ^^^^

Plutarch.hs:36:43: error: [GHC-83865]
    • Couldn't match type ‘PInteger’ with ‘PBool’
      Expected: Term s (PWithdraw :--> PBool)
        Actual: Term s (PWithdraw :--> PInteger)
    • In the first argument of ‘(#)’, namely ‘self’
      In the second argument of ‘(+)’, namely ‘(self # pfromData y)’
      In the expression: (self # pfromData x) + (self # pfromData y)
   |
36 |     PJoint x y -> (self # pfromData x) + (self # pfromData y)
   |                                           ^^^^

Plutarch.hs:37:24: error: [GHC-83865]
    • Couldn't match type ‘PInteger’ with ‘PBool’
      Expected: Term s (PWithdraw :--> PBool)
        Actual: Term s (PWithdraw :--> PInteger)
    • In the first argument of ‘(#)’, namely ‘self’
      In the first argument of ‘(-)’, namely ‘(self # pfromData from)’
      In the expression: (self # pfromData from) - pfromData n
   |
37 |     PDeduct n from -> (self # pfromData from) - pfromData n
   |                        ^^^^

Plutarch.hs:37:59: error: [GHC-83865]
    • Couldn't match type ‘PInteger’ with ‘PBool’
      Expected: Term s (PAsData PBool)
        Actual: Term s (PAsData PInteger)
    • In the first argument of ‘pfromData’, namely ‘n’
      In the second argument of ‘(-)’, namely ‘pfromData n’
      In the expression: (self # pfromData from) - pfromData n
   |
37 |     PDeduct n from -> (self # pfromData from) - pfromData n
   |                                                           ^</pre></td>
</tr>
</table>

### E06 — Apply a non-function

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:27:15: error: [GHC-83865]
    • Couldn't match expected type ‘t0 -> Integer’
                  with actual type ‘Integer’
    • The function ‘n’ is applied to one value argument,
        but its type ‘Integer’ has none
      In the expression: n 42
      In a case alternative: Amount n -> n 42
   |
27 |   Amount n -> n 42
   |               ^^^^</pre></td>
<td><pre>Plutarch.hs:35:28: error: [GHC-83865]
    • Couldn't match type ‘PInteger’ with ‘a0 :--> PInteger’
      Expected: Term s (PAsData (a0 :--> PInteger))
        Actual: Term s (PAsData PInteger)
    • In the first argument of ‘pfromData’, namely ‘n’
      In the first argument of ‘(#)’, namely ‘pfromData n’
      In the expression: pfromData n # 42
   |
35 |     PAmount n -> pfromData n # 42
   |                            ^</pre></td>
</tr>
</table>

### E07 — Wrong field from product

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:30:32: error: [GHC-83865]
    • Couldn't match expected type ‘Withdraw’
                  with actual type ‘Integer’
    • In the first argument of ‘netWithdraw’, namely ‘n’
      In the first argument of ‘(-)’, namely ‘netWithdraw n’
      In the expression: netWithdraw n - from
   |
30 |   Deduct n from -> netWithdraw n - from
   |                                ^

Plinth.hs:30:36: error: [GHC-83865]
    • Couldn't match expected type ‘Integer’
                  with actual type ‘Withdraw’
    • In the second argument of ‘(-)’, namely ‘from’
      In the expression: netWithdraw n - from
      In a case alternative: Deduct n from -> netWithdraw n - from
   |
30 |   Deduct n from -> netWithdraw n - from
   |                                    ^^^^</pre></td>
<td><pre>Plutarch.hs:38:41: error: [GHC-83865]
    • Couldn't match type ‘PInteger’ with ‘PWithdraw’
      Expected: Term s (PAsData PWithdraw)
        Actual: Term s (PAsData PInteger)
    • In the first argument of ‘pfromData’, namely ‘n’
      In the second argument of ‘(#)’, namely ‘pfromData n’
      In the first argument of ‘(-)’, namely ‘(self # pfromData n)’
   |
38 |     PDeduct n from -> (self # pfromData n) - pfromData from
   |                                         ^

Plutarch.hs:38:56: error: [GHC-83865]
    • Couldn't match type ‘PWithdraw’ with ‘PInteger’
      Expected: Term s (PAsData PInteger)
        Actual: Term s (PAsData PWithdraw)
    • In the first argument of ‘pfromData’, namely ‘from’
      In the second argument of ‘(-)’, namely ‘pfromData from’
      In the expression: (self # pfromData n) - pfromData from
   |
38 |     PDeduct n from -> (self # pfromData n) - pfromData from
   |                                                        ^^^^</pre></td>
</tr>
</table>

## Missing Constraints

### E08 — Missing equality constraint

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:11:17: error: [GHC-39999]
    • No instance for ‘Eq a’ arising from a use of ‘==’
      Possible fix:
        add (Eq a) to the context of
          the type signature for:
            isEqual :: forall a. a -> a -> Bool
    • In the expression: x == y
      In an equation for ‘isEqual’: isEqual x y = x == y
   |
11 | isEqual x y = x == y
   |                 ^^</pre></td>
<td><pre>Plutarch.hs:23:29: error: [GHC-39999]
    • No instance for ‘PEq a’ arising from a use of ‘#==’
      Possible fix:
        add (PEq a) to the context of
          the type signature for:
            pisEqual :: forall (s :: S) (a :: S -> *).
                        Term s (a :--> (a :--> PBool))
    • In the expression: x #== y
      In the second argument of ‘($)’, namely ‘\ x y -> x #== y’
      In the expression: plam $ \ x y -> x #== y
   |
23 | pisEqual = plam $ \x y -> x #== y
   |                             ^^^</pre></td>
</tr>
</table>

### E09 — Missing ordering constraint

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:12:8: error: [GHC-39999]
    • No instance for ‘Ord a’ arising from a use of ‘<’
      Possible fix:
        add (Ord a) to the context of
          the type signature for:
            clamp :: forall a. a -> a -> a -> a
    • In the expression: x < lo
      In the expression: if x < lo then lo else if x > hi then hi else x
      In an equation for ‘clamp’:
          clamp lo hi x = if x < lo then lo else if x > hi then hi else x
   |
12 |   if x < lo then lo
   |        ^</pre></td>
<td><pre>Plutarch.hs:24:10: error: [GHC-39999]
    • No instance for ‘POrd a’ arising from a use of ‘#<’
      Possible fix:
        add (POrd a) to the context of
          the type signature for:
            pclamp :: forall (s :: S) (a :: S -> *).
                      Term s (a :--> (a :--> (a :--> a)))
    • In the first argument of ‘pif’, namely ‘(x #< lo)’
      In the expression: pif (x #< lo) lo (pif (hi #< x) hi x)
      In the second argument of ‘($)’, namely
        ‘\ lo hi x -> pif (x #< lo) lo (pif (hi #< x) hi x)’
   |
24 |   pif (x #< lo) lo (pif (hi #< x) hi x)
   |          ^^</pre></td>
</tr>
</table>

### E10 — Missing Data-conversion constraint

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:25:18: error: [GHC-39999]
    • No instance for ‘ToData a’ arising from a use of ‘DJust’
      Possible fix:
        add (ToData a) to the context of
          the type signature for:
            fromMaybeData :: forall a. a -> MaybeData a -> a
    • In the pattern: DJust x
      In an equation for ‘fromMaybeData’: fromMaybeData _ (DJust x) = x
   |
25 | fromMaybeData _ (DJust x) = x
   |                  ^^^^^^^</pre></td>
<td><pre>Plutarch.hs:36:17: error: [GHC-39999]
    • No instance for ‘PIsData a’ arising from a use of ‘pfromData’
      Possible fix:
        add (PIsData a) to the context of
          the type signature for:
            pfromMaybeData :: forall (s :: S) (a :: S -> *).
                              Term s (a :--> (PMaybeData a :--> a))
    • In the expression: pfromData x
      In a \case alternative: PDJust x -> pfromData x
      In the second argument of ‘($)’, namely
        ‘\case
           PDNothing -> def
           PDJust x -> pfromData x’
   |
36 |     PDJust x -> pfromData x
   |                 ^^^^^^^^^</pre></td>
</tr>
</table>

## Precedence and Application

### E11 — Missing parentheses around constructor pattern

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:14:1: error: [GHC-91938]
    equations for ‘foldList’ have different numbers of arguments
      Plinth.hs:14:1-24
      Plinth.hs:16:1-54
   |
14 | foldList _ acc Nil = acc
   | ^^^^^^^^^^^^^^^^^^^^^^^^...</pre></td>
<td><pre>Plutarch.hs:33:1: error: [GHC-91938]
    equations for ‘isAmount’ have different numbers of arguments
      Plutarch.hs:33:1-31
      Plutarch.hs:34:1-24
   |
33 | isAmount PAmount _ = pcon PTrue
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...</pre></td>
</tr>
</table>

### E12 — Missing parentheses in function chain

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:30:20: error: [GHC-83865]
    • Couldn't match expected type ‘Integer -> Withdraw -> Integer’
                  with actual type ‘Integer’
    • The function ‘netWithdraw’ is applied to three value arguments,
        but its type ‘Withdraw -> Integer’ has only one
      In the expression: netWithdraw Deduct n from
      In a case alternative: Deduct n from -> netWithdraw Deduct n from
   |
30 |   Deduct n from -> netWithdraw Deduct n from
   |                    ^^^^^^^^^^^^^^^^^^^^^^^^^

Plinth.hs:30:32: error: [GHC-83865]
    • Couldn't match expected type ‘Withdraw’
                  with actual type ‘Integer -> Withdraw -> Withdraw’
    • Probable cause: ‘Deduct’ is applied to too few arguments
      In the first argument of ‘netWithdraw’, namely ‘Deduct’
      In the expression: netWithdraw Deduct n from
      In a case alternative: Deduct n from -> netWithdraw Deduct n from
   |
30 |   Deduct n from -> netWithdraw Deduct n from
   |                                ^^^^^^</pre></td>
<td><pre>Plutarch.hs:38:24: error: [GHC-83865]
    • Couldn't match type ‘PInteger’
                     with ‘PAsData PWithdraw :--> PInteger’
      Expected: Term s (PWithdraw :--> (PAsData PWithdraw :--> PInteger))
        Actual: Term s (PWithdraw :--> PInteger)
    • In the first argument of ‘(#)’, namely ‘self’
      In the first argument of ‘(#)’, namely ‘self # pfromData’
      In the first argument of ‘(-)’, namely ‘(self # pfromData # from)’
   |
38 |     PDeduct n from -> (self # pfromData # from) - pfromData n
   |                        ^^^^

Plutarch.hs:38:31: error: [GHC-83865]
    • Couldn't match expected type: Term s PWithdraw
                  with actual type: Term s0 (PAsData a0) -> Term s0 a0
    • Probable cause: ‘pfromData’ is applied to too few arguments
      In the second argument of ‘(#)’, namely ‘pfromData’
      In the first argument of ‘(#)’, namely ‘self # pfromData’
      In the first argument of ‘(-)’, namely ‘(self # pfromData # from)’
    • Relevant bindings include
        from :: Term s (PAsData PWithdraw)
          (bound at Plutarch.hs:38:15)
        n :: Term s (PAsData PInteger)
          (bound at Plutarch.hs:38:13)
        w :: PWithdraw s
          (bound at Plutarch.hs:34:55)
        wt :: Term s PWithdraw
          (bound at Plutarch.hs:34:36)
        self :: Term s (PWithdraw :--> PInteger)
          (bound at Plutarch.hs:34:31)
        netWithdraw :: Term s (PWithdraw :--> PInteger)
          (bound at Plutarch.hs:34:1)
   |
38 |     PDeduct n from -> (self # pfromData # from) - pfromData n
   |                               ^^^^^^^^^</pre></td>
</tr>
</table>

### E13 — Operator section confusion

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:28:15: error: [GHC-83865]
    • Couldn't match expected type ‘(Integer -> Integer -> Integer)
                                    -> t0 -> Integer’
                  with actual type ‘Integer’
    • The function ‘n’ is applied to two value arguments,
        but its type ‘Integer’ has none
      In the expression: n modulo 2
      In a case alternative: Amount n -> n modulo 2
   |
28 |   Amount n -> n modulo 2
   |               ^^^^^^^^^^</pre></td>
<td><pre>Plutarch.hs:38:9: error: [GHC-83865]
    • Couldn't match expected type: Term
                                      s0 (PInteger :--> (PInteger :--> PInteger))
                                    -> t0 -> Term s PInteger
                  with actual type: Term s PInteger
    • The function ‘n'’ is applied to two value arguments,
        but its type ‘Term s PInteger’ has none
      In the expression: n' pmod 2
      In the second argument of ‘($)’, namely ‘\ n' -> n' pmod 2’
    • Relevant bindings include
        n' :: Term s PInteger
          (bound at Plutarch.hs:37:29)
        n :: Term s (PAsData PInteger)
          (bound at Plutarch.hs:36:13)
        w :: PWithdraw s
          (bound at Plutarch.hs:34:55)
        wt :: Term s PWithdraw
          (bound at Plutarch.hs:34:36)
        self :: Term s (PWithdraw :--> PInteger)
          (bound at Plutarch.hs:34:31)
        netWithdraw :: Term s (PWithdraw :--> PInteger)
          (bound at Plutarch.hs:34:1)
   |
38 |         n' pmod 2
   |         ^^^^^^^^^</pre></td>
</tr>
</table>

## Naming and Scope

### E14 — Typo in function name

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:27:17: error: [GHC-88464]
    Variable not in scope: modul :: Integer -> t0 -> Integer
    Suggested fix:
      Perhaps use ‘modulo’ (imported from PlutusTx.Prelude)
   |
27 |   Amount n -> n `modul` 2
   |                 ^^^^^^^</pre></td>
<td><pre>Plutarch.hs:35:18: error: [GHC-88464]
    Variable not in scope:
      pfromDat :: Term s (PAsData PInteger) -> Term s PInteger
    Suggested fix:
      Perhaps use ‘pfromData’ (imported from Plutarch.Prelude)
   |
35 |     PAmount n -> pfromDat n
   |                  ^^^^^^^^</pre></td>
</tr>
</table>

### E15 — Lowercase constructor

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:27:3: error: [GHC-07626]
    Parse error in pattern: amount
   |
27 |   amount n -> n
   |   ^^^^^^^^</pre></td>
<td><pre>Plutarch.hs:35:5: error: [GHC-07626]
    Parse error in pattern: pAmount
   |
35 |     pAmount n -> pfromData n
   |     ^^^^^^^^^</pre></td>
</tr>
</table>

### E16 — Variable not in scope

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:29:39: error: [GHC-88464]
    Variable not in scope: amount :: Integer
    Suggested fix: Perhaps use data constructor ‘Amount’ (line 15)
   |
29 |   Deduct n from -> netWithdraw from - amount
   |                                       ^^^^^^</pre></td>
<td><pre>Plutarch.hs:37:59: error: [GHC-88464]
    Variable not in scope: amount :: Term s (PAsData PInteger)
    Suggested fix: Perhaps use data constructor ‘PAmount’ (line 24)
   |
37 |     PDeduct n from -> (self # pfromData from) - pfromData amount
   |                                                           ^^^^^^</pre></td>
</tr>
</table>

## Pattern Matching and Exhaustiveness

### E17 — Missing base case

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:7:1: warning: [-Wunused-imports]
    The import of ‘PlutusTx.Prelude’ is redundant
      except perhaps to import instances from ‘PlutusTx.Prelude’
    To import instances alone, use: import PlutusTx.Prelude()
  |
7 | import PlutusTx.Prelude
  | ^^^^^^^^^^^^^^^^^^^^^^^

Plinth.hs:15:1: warning: [GHC-62161] [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘foldList’:
        Patterns of type ‘b -> a -> b’, ‘b’, ‘List a’ not matched: _ _ Nil
   |
15 | foldList f acc (Cons x rest) = foldList f (f acc x) rest
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^</pre></td>
<td><pre>Plutarch.hs:34:15: warning: [GHC-62161] [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a \case alternative:
        Patterns of type ‘PList a s’ not matched: PNil
   |
34 |   pmatch xs $ \case
   |               ^^^^^...</pre></td>
</tr>
</table>

### E18 — Missing constructor in match

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:26:17: warning: [GHC-62161] [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative:
        Patterns of type ‘Withdraw’ not matched:
            Withdraw_6989586621679031498
            (PlutusTx.Builtins.Internal.BuiltinData _)
   |
26 | netWithdraw w = case w of
   |                 ^^^^^^^^^...</pre></td>
<td><pre>Plutarch.hs:34:3: warning: [GHC-62161] [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative:
        Patterns of type ‘PWithdraw s’ not matched: PDeduct _ _
   |
34 |   case w of
   |   ^^^^^^^^^...</pre></td>
</tr>
</table>

### E19 — Wrong number of fields in pattern

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:28:3: error:
    • The constructor ‘Joint’ should have 2 arguments, but has been given 1
    • In the pattern: Joint x
      In a case alternative: Joint x -> netWithdraw x
      In the expression:
        case w of
          Amount n -> n
          Joint x -> netWithdraw x
          Deduct n from -> netWithdraw from - n
   |
28 |   Joint x -> netWithdraw x
   |   ^^^^^^^</pre></td>
<td><pre>Plutarch.hs:36:5: error:
    • The constructor ‘PJoint’ should have 2 arguments, but has been given 1
    • In the pattern: PJoint x
      In a case alternative: PJoint x -> self # pfromData x
      In the expression:
        case w of
          PAmount n -> pfromData n
          PJoint x -> self # pfromData x
          PDeduct n from -> (self # pfromData from) - pfromData n
   |
36 |     PJoint x -> self # pfromData x
   |     ^^^^^^^^</pre></td>
</tr>
</table>

## Wrong Function / Wrong Operation

### E20 — Confuse list operations

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:16:22: error: [GHC-25897]
    • Couldn't match expected type ‘a’ with actual type ‘List a’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          prepend :: forall a. List a -> List a -> List a
        at Plinth.hs:15:1-37
    • In the first argument of ‘Cons’, namely ‘xs’
      In the expression: Cons xs ys
      In an equation for ‘prepend’: prepend xs ys = Cons xs ys
    • Relevant bindings include
        ys :: List a
          (bound at Plinth.hs:16:12)
        xs :: List a
          (bound at Plinth.hs:16:9)
        prepend :: List a -> List a -> List a
          (bound at Plinth.hs:16:1)
   |
16 | prepend xs ys = Cons xs ys
   |                      ^^</pre></td>
<td><pre>Plutarch.hs:34:12: error: [GHC-25897]
    • Couldn't match type ‘a’ with ‘PList a’
        arising from a use of ‘plam’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          pprepend :: forall (s :: S) (a :: S -> *).
                      Term s (PList a :--> (PList a :--> PList a))
        at Plutarch.hs:33:1-54
    • In the first argument of ‘($)’, namely ‘plam’
      In the expression: plam $ \ xs ys -> pcon (PCons xs ys)
      In an equation for ‘pprepend’:
          pprepend = plam $ \ xs ys -> pcon (PCons xs ys)
    • Relevant bindings include
        pprepend :: Term s (PList a :--> (PList a :--> PList a))
          (bound at Plutarch.hs:34:1)
   |
34 | pprepend = plam $ \xs ys -> pcon (PCons xs ys)
   |            ^^^^</pre></td>
</tr>
</table>

### E21 — Wrong Prelude function on custom type

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:15:15: error: [GHC-88464]
    Variable not in scope: length :: List Integer -> Integer
   |
15 | myLength xs = length xs
   |               ^^^^^^</pre></td>
<td><pre>Plutarch.hs:33:27: error: [GHC-05617]
    • Could not solve: ‘PElemConstraint PList PInteger’
        arising from a use of ‘plength’
    • In the first argument of ‘(#)’, namely ‘plength’
      In the expression: plength # xs
      In the second argument of ‘($)’, namely ‘\ xs -> plength # xs’
   |
33 | pmyLength = plam $ \xs -> plength # xs
   |                           ^^^^^^^

Plutarch.hs:33:27: error: [GHC-39999]
    • No instance for ‘PListLike PList’ arising from a use of ‘plength’
      There are instances for similar types:
        instance PListLike Plutarch.List.PList
          -- Defined in ‘Plutarch.List’
    • In the first argument of ‘(#)’, namely ‘plength’
      In the expression: plength # xs
      In the second argument of ‘($)’, namely ‘\ xs -> plength # xs’
   |
33 | pmyLength = plam $ \xs -> plength # xs
   |                           ^^^^^^^</pre></td>
</tr>
</table>

### E22 — Equality test on function type

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:32:20: error: [GHC-39999]
    • No instance for ‘Eq (Withdraw -> Integer)’
        arising from a use of ‘==’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression: netWithdraw == netWithdraw
      In an equation for ‘same’: same = netWithdraw == netWithdraw
   |
32 | same = netWithdraw == netWithdraw
   |                    ^^</pre></td>
<td><pre>Plutarch.hs:40:20: error: [GHC-39999]
    • No instance for ‘PEq (PWithdraw :--> PInteger)’
        arising from a use of ‘#==’
    • In the expression: netWithdraw #== netWithdraw
      In an equation for ‘same’: same = netWithdraw #== netWithdraw
   |
40 | same = netWithdraw #== netWithdraw
   |                    ^^^</pre></td>
</tr>
</table>

## Recursion

### E23 — Wrong pattern order

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:7:1: warning: [-Wunused-imports]
    The import of ‘PlutusTx.Prelude’ is redundant
      except perhaps to import instances from ‘PlutusTx.Prelude’
    To import instances alone, use: import PlutusTx.Prelude()
  |
7 | import PlutusTx.Prelude
  | ^^^^^^^^^^^^^^^^^^^^^^^

Plinth.hs:16:1: warning: [GHC-53633] [-Woverlapping-patterns]
    Pattern match is redundant
    In an equation for ‘foldList’: foldList f acc (Cons x rest) = ...
   |
16 | foldList f acc (Cons x rest) = foldList f (f acc x) rest
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^</pre></td>
<td><pre>Plutarch.hs:36:5: warning: [GHC-53633] [-Woverlapping-patterns]
    Pattern match is redundant
    In a \case alternative: PCons x rest -> ...
   |
36 |     PCons x rest -> self # f # (f # acc # x) # rest
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^</pre></td>
</tr>
</table>

### E24 — Self-reference without proper recursion

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:29:16: error: [GHC-83865]
    • Couldn't match expected type ‘Withdraw -> Integer’
                  with actual type ‘Withdraw’
    • The function ‘netWithdraw’ is applied to one value argument,
        but its type ‘Withdraw’ has none
      In the first argument of ‘(+)’, namely ‘netWithdraw x’
      In the expression: netWithdraw x + netWithdraw y
   |
29 |   Joint x y -> netWithdraw x + netWithdraw y
   |                ^^^^^^^^^^^^^

Plinth.hs:29:32: error: [GHC-83865]
    • Couldn't match expected type ‘Withdraw -> Integer’
                  with actual type ‘Withdraw’
    • The function ‘netWithdraw’ is applied to one value argument,
        but its type ‘Withdraw’ has none
      In the second argument of ‘(+)’, namely ‘netWithdraw y’
      In the expression: netWithdraw x + netWithdraw y
   |
29 |   Joint x y -> netWithdraw x + netWithdraw y
   |                                ^^^^^^^^^^^^^

Plinth.hs:30:20: error: [GHC-83865]
    • Couldn't match expected type ‘Withdraw -> Integer’
                  with actual type ‘Withdraw’
    • The function ‘netWithdraw’ is applied to one value argument,
        but its type ‘Withdraw’ has none
      In the first argument of ‘(-)’, namely ‘netWithdraw from’
      In the expression: netWithdraw from - n
   |
30 |   Deduct n from -> netWithdraw from - n
   |                    ^^^^^^^^^^^^^^^^</pre></td>
<td><pre>Plutarch.hs:37:20: error: [GHC-83865]
    • Couldn't match type ‘PWithdraw’ with ‘PWithdraw :--> PInteger’
      Expected: Term s (PWithdraw :--> PInteger)
        Actual: Term s PWithdraw
    • In the first argument of ‘(#)’, namely ‘netWithdraw’
      In the first argument of ‘(+)’, namely
        ‘(netWithdraw # pfromData x)’
      In the expression:
        (netWithdraw # pfromData x) + (netWithdraw # pfromData y)
   |
37 |     PJoint x y -> (netWithdraw # pfromData x) + (netWithdraw # pfromData y)
   |                    ^^^^^^^^^^^

Plutarch.hs:37:50: error: [GHC-83865]
    • Couldn't match type ‘PWithdraw’ with ‘PWithdraw :--> PInteger’
      Expected: Term s (PWithdraw :--> PInteger)
        Actual: Term s PWithdraw
    • In the first argument of ‘(#)’, namely ‘netWithdraw’
      In the second argument of ‘(+)’, namely
        ‘(netWithdraw # pfromData y)’
      In the expression:
        (netWithdraw # pfromData x) + (netWithdraw # pfromData y)
   |
37 |     PJoint x y -> (netWithdraw # pfromData x) + (netWithdraw # pfromData y)
   |                                                  ^^^^^^^^^^^

Plutarch.hs:38:24: error: [GHC-83865]
    • Couldn't match type ‘PWithdraw’ with ‘PWithdraw :--> PInteger’
      Expected: Term s (PWithdraw :--> PInteger)
        Actual: Term s PWithdraw
    • In the first argument of ‘(#)’, namely ‘netWithdraw’
      In the first argument of ‘(-)’, namely
        ‘(netWithdraw # pfromData from)’
      In the expression: (netWithdraw # pfromData from) - pfromData n
   |
38 |     PDeduct n from -> (netWithdraw # pfromData from) - pfromData n
   |                        ^^^^^^^^^^^</pre></td>
</tr>
</table>

## Type System Edge Cases

### E25 — Infinite type

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:21:52: error: [GHC-83865]
    • Couldn't match expected type ‘List a0’ with actual type ‘Integer’
    • In the third argument of ‘foldList’, namely ‘x’
      In the first argument of ‘(+)’, namely
        ‘foldList (\ acc _ -> acc) 0 x’
      In the expression: foldList (\ acc _ -> acc) 0 x + badFold rest
   |
21 | badFold (Cons x rest) = foldList (\acc _ -> acc) 0 x + badFold rest
   |                                                    ^</pre></td>
<td><pre>Plutarch.hs:40:20: error: [GHC-18872]
    • Couldn't match type ‘PList a0’ with ‘PInteger’
        arising from a use of ‘plam’
    • In the first argument of ‘($)’, namely ‘plam’
      In the second argument of ‘(#$)’, namely
        ‘plam
           $ \ self xs
               -> pmatch xs
                    $ \case
                        PNil -> 0
                        PCons x rest
                          -> (pfoldList # plam (\ acc _ -> acc) # 0 # x) + (self # rest)’
      In the expression:
        pfix
          #$
            plam
              $ \ self xs
                  -> pmatch xs
                       $ \case
                           PNil -> 0
                           PCons x rest
                             -> (pfoldList # plam (\ acc _ -> acc) # 0 # x) + (self # rest)
   |
40 | pbadFold = pfix #$ plam $ \self xs ->
   |                    ^^^^

Plutarch.hs:43:74: error: [GHC-83865]
    • Couldn't match type ‘PList a0’ with ‘PInteger’
      Expected: Term s (PList PInteger)
        Actual: Term s (PList (PList a0))
    • In the second argument of ‘(#)’, namely ‘rest’
      In the second argument of ‘(+)’, namely ‘(self # rest)’
      In the expression:
        (pfoldList # plam (\ acc _ -> acc) # 0 # x) + (self # rest)
    • Relevant bindings include
        rest :: Term s (PList (PList a0))
          (bound at Plutarch.hs:43:13)
        x :: Term s (PList a0)
          (bound at Plutarch.hs:43:11)
        xs :: Term s (PList (PList a0))
          (bound at Plutarch.hs:40:33)
   |
43 |     PCons x rest -> (pfoldList # plam (\acc _ -> acc) # 0 # x) + (self # rest)
   |                                                                          ^^^^</pre></td>
</tr>
</table>

### E26 — Ambiguous type variable

<table>
<tr><th>Plinth</th><th>Plutarch</th></tr>
<tr>
<td><pre>Plinth.hs:11:15: error: [GHC-83865]
    • Couldn't match expected type ‘Integer’
                  with actual type ‘BuiltinData’
    • In the expression: toBuiltinData (unsafeFromBuiltinData d)
      In an equation for ‘roundtrip’:
          roundtrip d = toBuiltinData (unsafeFromBuiltinData d)
   |
11 | roundtrip d = toBuiltinData (unsafeFromBuiltinData d)
   |               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^</pre></td>
<td><pre>Plutarch.hs:23:14: error: [GHC-18872]
    • Couldn't match type ‘PAsData a0’ with ‘PInteger’
        arising from a use of ‘plam’
    • In the first argument of ‘($)’, namely ‘plam’
      In the expression: plam $ \ d -> pdata (pfromData d)
      In an equation for ‘proundtrip’:
          proundtrip = plam $ \ d -> pdata (pfromData d)
   |
23 | proundtrip = plam $ \d -> pdata (pfromData d)
   |              ^^^^

Plutarch.hs:23:44: error: [GHC-83865]
    • Couldn't match type ‘PData’ with ‘PAsData a0’
      Expected: Term s (PAsData a0)
        Actual: Term s PData
    • In the first argument of ‘pfromData’, namely ‘d’
      In the first argument of ‘pdata’, namely ‘(pfromData d)’
      In the expression: pdata (pfromData d)
   |
23 | proundtrip = plam $ \d -> pdata (pfromData d)
   |                                            ^</pre></td>
</tr>
</table>
