# Top-8 error message rig

Each `EXX-*/` directory holds a `Plinth.patch` (and, where applicable, a
`Plutarch.patch`) that, when applied to the corresponding real validator
under `src/`, produces a buggy source file. The bug recreates one of the
top-8 diagnostics from `error-message-comparison-top8.md` in real
validator code.

## Layout

```
src-errormsg-top8/
  E04-DivisionOnIntegers/
    Plinth.patch       # unified diff against src/Vesting/Contracts/VestingPlinth.hs
    Plutarch.patch     # unified diff against src/Vesting/Contracts/Vesting.hs
    Plinth.hs          # generated: cp source + patch
    Plutarch.hs        # generated: cp source + patch
    Plinth.err         # captured GHC diagnostics
    Plutarch.err       # captured GHC diagnostics
  ...
  run.sh             # generate (cp + patch) + compile + capture diagnostics
```

## Validator chosen per case

| Case | Real validator copied | Why |
|------|----------------------|-----|
| E04  | Vesting (`Vesting.hs` / `VestingPlinth.hs`) | Only validator in the suite that performs integer division (`linearVesting`). |
| E05  | Vesting                                     | `linearVesting` has a clean `Integer`-returning branch to swap to `Bool`. |
| E07  | Vesting                                     | The `TxOut` / `PTxOut` destructure in `vestingValidator` binds two same-shape positional fields (address, value); swapping the local bindings makes every downstream use cross types, producing two symmetric "Couldn't match" diagnostics. |
| E12  | Vesting                                     | Has both a Plinth constructor application (`unsafeFromBuiltinData @VestingRedeemer (getRedeemer redeemer)`) and a Plutarch `#`-chain (`pmatch (pfromData inp)`). |
| E21  | Voting (`Voting.hs` / `VotingPlinth.hs`) + dead-code helper | Smallest validator pair. The custom-`List` / local-`PList` shadow doesn't occur naturally in any validator, so the helper is appended to a real validator rather than fabricated as a toy. |
| E25  | Vesting                                     | `(p)foldAdaByVkhOutputs` are the only recursive folds in the suite where swapping the element/list arg induces the occurs check. |
| E27  | Voting Plinth + dead-code GADT              | Plinth-only. The minimal `data Tag a where ...` is appended to the smallest Plinth validator and fed through `plinthc`. |
| E29  | Voting Plinth + `plinthc` closure           | Plinth-only. A helper that hands `plinthc` a lambda capturing the surrounding `Integer` parameter; the closure's `x` is not a local/builtin/INLINABLE binding from the plugin's point of view. |

## Workflow

```bash
nix develop                                # provides GHC + cabal + project deps
src-errormsg-top8/run.sh                   # generate + compile + collect, every case
src-errormsg-top8/run.sh E04 E07           # restrict to a subset
```

`run.sh` is the **only** script in this directory. For every
`EXX/Variant.patch` it:

1. **Generates** `EXX/Variant.hs` by reading the `# source:` header from the
   patch, `cp`-ing that upstream validator from `src/`, and applying the
   diff with `patch -p1`. The `.patch` files are the source of truth for
   which bug is injected.
2. **Compiles** the generated file with
   `cabal exec ghc -- -Wall -fforce-recomp -c -package plinth-plutarch-paper-code`
   plus the project's default-extensions, capturing stdout+stderr.
3. **Collects** the filtered diagnostic (cabal/nix preamble stripped) into
   the sibling `EXX/Variant.err`.

Per-case verdict printed on stdout is one of:

* `errors captured`        — GHC failed (expected for every case in the top-8 set).
* `warnings captured`      — GHC succeeded but emitted at least one warning.
* `clean (no diagnostics)` — the bug didn't fire.

## Where the captured error goes

Every captured GHC diagnostic lands in `EXX-*/Variant.err`, sitting next to
the `Variant.hs` that produced it. Nothing is written outside the case
directory. Build artifacts (`.hi`/`.o`/`.build/`) are produced under each
case directory but `.gitignore`'d.

For the top 8 specifically:

```
E04-DivisionOnIntegers/{Plinth,Plutarch}.err
E05-BoolForInteger/{Plinth,Plutarch}.err
E12-MissingAppParens/{Plinth,Plutarch}.err
E07-WrongField/{Plinth,Plutarch}.err
E21-WrongPreludeFunction/{Plinth,Plutarch}.err
E25-InfiniteType/{Plinth,Plutarch}.err
E27-UnsupportedGADTs/Plinth.err
E29-StageError/Plinth.err
```

## What each diagnostic should look like

The `*.err` files reproduce the signatures documented in
`../error-message-comparison-top8.md`:

* **E04**: Plinth `Variable not in scope: (/) :: Integer -> Integer -> Integer`;
  Plutarch `No instance for ‘Fractional (Term s PInteger)’`.
* **E05**: Plinth `Couldn't match expected type ‘Integer’ with actual type ‘Bool’`;
  Plutarch `Couldn't match type ‘Bool’ with ‘Integer’` (in `pconstant`).
* **E07**: Plinth two symmetric `Couldn't match expected type ‘Value’ with
  actual type ‘Address’` / `‘Address’ with ‘Value’` diagnostics at the
  downstream uses of the swapped bindings; Plutarch the analogous
  `Couldn't match type ‘PAddress’ with ‘PAsData (PValue …)’` pair at
  `pfromData ptxOut'value` and at `pgetOutputsByAddress # … # ptxOut'address`.
* **E12**: Plinth `Function 'unsafeFromBuiltinData' is applied to two value
  arguments, but its type 'BuiltinData -> VestingRedeemer' has only one`;
  Plutarch `‘pfromData’ is applied to too few arguments` (extra `#` step).
* **E21**: Plinth `Variable not in scope: length :: UserList Integer -> Integer`;
  Plutarch `Couldn't match type ‘PUserList’ with ‘list0 a0’` (no
  `PListLike` instance).
* **E25**: Plinth `Couldn't match expected type ‘Integer’ with actual type ‘List TxOut’`;
  Plutarch `Couldn't match type: PAsData PTxOut with: PBuiltinList (PAsData PTxOut)`.
* **E27**: Plinth `Unsupported feature: Following extensions are not supported: GADTs`.
* **E29**: Plinth `Reference to a name which is not a local, a builtin,
  or an external INLINABLE function: Variable x`, preceded by the
  `Context: Compiling code:` GHC Core trace.

## When a validator under `src/` changes

If the bug-line moves and `patch` rejects the diff, regenerate the failing
`.patch` by hand: edit the buggy area in a fresh copy of the upstream
file, then `diff -u src/<Validator>.hs <copy>.hs > Plinth.patch` (preserve
the `# source:` header at the top). The `.patch` is just a unified diff —
nothing magic.
