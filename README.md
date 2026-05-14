# Plinth Plutarch Paper Codes

This repository ports 7 Cardano smart contracts to both Plinth and Plutarch from their original implementations. The table below summarizes each contract's script purpose, original source, and the language it was originally written in.

| Validator | Script Purpose | Original Source | Original Language | Description |
|---|---|---|---|---|
| [Smart Tokens](./src/SmartTokens) | Rewarding | [input-output-hk/wsc-poc](https://github.com/input-output-hk/wsc-poc) | Plutarch | ERC-20 style programmable tokens |
| [Guardrail](./src/Constitution) | Proposing | [IntersectMBO/plutus &mdash; cardano-constitution](https://github.com/IntersectMBO/plutus/tree/master/cardano-constitution) | Plinth | Guardrails for governance proposals |
| [Crowdfund](./src/Crowdfund) | Spending | [blockchain-unica/rosetta-smart-contracts](https://github.com/blockchain-unica/rosetta-smart-contracts) | Aiken | Fundraising with refund |
| [Vesting](./src/Vesting) | Spending | [blockchain-unica/rosetta-smart-contracts](https://github.com/blockchain-unica/rosetta-smart-contracts) | Aiken | Time-locked release of funds |
| [SundaeSwap NFT](./src/Settings) | Minting | [SundaeSwap-finance/sundae-contracts](https://github.com/SundaeSwap-finance/sundae-contracts) | Aiken | Minting of authorizing NFT |
| [Certifying](./src/Certifying) | Certifying | [SundaeSwap-finance/treasury-contracts &mdash; treasury.ak](https://github.com/SundaeSwap-finance/treasury-contracts/blob/main/validators/treasury.ak#L83), [vendor.ak](https://github.com/SundaeSwap-finance/treasury-contracts/blob/main/validators/vendor.ak#L57) | Aiken | Vote-delegation certificate validation |
| [Voting](./src/Voting) | Voting | [IntersectMBO/credential-manager &mdash; HotCommittee.hs](https://github.com/IntersectMBO/credential-manager/blob/main/credential-manager/src/CredentialManager/Scripts/HotCommittee.hs) | Plinth | Hot committee credential script |

All data is collected using Plinth compiler version `1.64.0.0` and Plutarch version `1.12.0`. Both Plinth and Plutarch depend on `plutus-core`, which provides the UPLC virtual machine that executes scripts and measures their execution budget. To make both languages buildable within a single project, we use a fork of Plutarch. The changes in the fork are minimal and self-contained: they only adapt Plutarch to interface changes in the `plutus-core` library.

## Benchmarks

All numbers below were produced by the executables in this repository:

- **Source size (eLOC)** &mdash; `cabal run measure-code-size`. eLOC subtracts pragmas, module head, imports, and comments from total lines.
- **Script size (bytes)** and **execution cost (CPU steps, memory units)** &mdash; `cabal run bench-scripts`. Sizes are serialized UPLC; execution costs are reported by the `plutus-core` CEK evaluator.
- **Source program jargon level** &mdash; `cabal run measure-jargon-level`. Counts occurrences in `Contracts/*.hs` of DSL-specific identifiers, types, and operators that originate in each language's libraries (e.g. `plet`, `pmatch`, `#==`, `:-->`, `PScriptContext` for Plutarch; `plinthc`, `unsafeFromBuiltinData`, `BI.*`, `DList.*`, `ScriptContext` for Plinth). The two curated lists are kept structurally parallel so every P-prefixed Plutarch ledger type has its unprefixed Plinth counterpart.

Toolchain: Plinth `1.64.0.0`, Plutarch `1.12.0`, GHC `9.6.6`. All ratios are **Plinth / Plutarch** (values below `1.00x` indicate Plinth is smaller or cheaper). 

### Script Sizes and eLOC

| Validator      | Plutarch eLOC | Plinth eLOC | eLOC ratio | Plutarch Size (Bytes) | Plinth Size | Size ratio |
|----------------|--------------:|------------:|-----------:|------------------:|----------------:|-----------:|
| Smart Tokens   |           985 |         577 |      0.59x |              4080 |            3902 |      0.96x |
| Guardrail      |           202 |          89 |      0.44x |               769 |             798 |      1.04x |
| Crowdfund      |           303 |         211 |      0.70x |              1888 |            2203 |      1.17x |
| Vesting        |           199 |         155 |      0.78x |              1219 |            1184 |      0.97x |
| SundaeSwap NFT |           345 |         220 |      0.64x |              2162 |            2475 |      1.14x |
| Certifying     |            52 |          36 |      0.69x |               317 |             381 |      1.20x |
| Voting         |            55 |          58 |      1.05x |               272 |             244 |      0.90x |
| **Total**      |      **2141** |    **1346** |  **0.63x** |         **10707** |       **11187** |  **1.04x** |

### Source Program Jargon Level

This metric counts occurrences of DSL-specific identifiers and operators in `Contracts/*.hs`, approximating the vocabulary a reader must learn. Each file is parsed and every name reference is checked against a curated jargon list for its DSL. Only library-imported tokens count: user-defined helpers (e.g. `pcheck`) and user-defined types (e.g. `PVestingDatum`) are excluded, as are pragmas, headers, imports, and comments. The per-line density column uses eLOC from the previous section as the denominator.

The two curated lists are designed to be symmetric. Plutarch's term primitives (`plam`, `plet`, `pmatch`, `pif`, `pfix`, `pelimList`, `phoistAcyclic`, `pfromData`, `pasConstr`, `pfstBuiltin`, ...) are paired against Plinth's term primitives (`plinthc`, `unsafeFromBuiltinData`, `matchInterval`, `matchOutputDatum`, ...) and qualified-builtin prefixes (`BI.*`, `Builtins.*`, `DList.*`, `DMap.*`). Plutarch's operators (`#`, `#$`, `#==`, `#/=`, `#<`, `#<=`, `#>`, `#>=`, `#&&`, `#||`, `:-->`) have no Plinth analogue and contribute only to the Plutarch side. Every P-prefixed Plutarch ledger type (`PScriptContext`, `PTxInfo`, `PTxOut`, `PValue`, `PAddress`, `PPubKeyHash`, all `P{Spending,Voting,...}Script`, `PInterval`, `POutputDatum`, ...) has its unprefixed Plinth counterpart counted on the other side.

| Validator      | Plutarch Jargon | Plinth Jargon | Jargon ratio | Plutarch /eLOC | Plinth /eLOC |
|----------------|----------------:|--------------:|-------------:|---------------:|-------------:|
| Smart Tokens   |            1724 |           350 |        0.20x |           1.75 |         0.61 |
| Guardrail      |             266 |            44 |        0.17x |           1.32 |         0.49 |
| Crowdfund      |             491 |           112 |        0.23x |           1.62 |         0.53 |
| Vesting        |             295 |            89 |        0.30x |           1.48 |         0.57 |
| SundaeSwap NFT |             576 |           106 |        0.18x |           1.67 |         0.48 |
| Certifying     |             112 |            22 |        0.20x |           2.15 |         0.61 |
| Voting         |             156 |            46 |        0.29x |           2.84 |         0.79 |
| **Total**      |        **3620** |       **769** |    **0.21x** |       **1.69** |     **0.57** |

The aggregate Plutarch program emits roughly three times as many jargon tokens per effective line of code as the aggregate Plinth program (1.69 vs 0.57), and 4.7 times the raw absolute count (3,620 vs 769) once the eLOC difference is folded in. The gap is consistent across every validator in the suite: Plutarch's per-eLOC density ranges from 1.32 to 2.84, Plinth's from 0.48 to 0.79. Voting is the only pair where Plinth's eLOC slightly exceeds Plutarch's, yet Plinth's jargon density there (0.79) is still less than a third of Plutarch's (2.84).

### Execution Cost &mdash; Per-Scenario

Only accepting scenarios are reported below. Rejecting scenarios are excluded because the two implementations may short-circuit at different program points, so their costs do not measure equivalent work. This restriction also reflects the Cardano execution model faithfully: a transaction is pre-evaluated off-chain before submission to confirm that its scripts succeed, so failing scripts never run on-chain and never incur fees.

Rejecting cases and some property based test cases are still exercised by the test suite to ensure that the Plinth and Plutarch implementations agree on every validation outcome, accepting and rejecting alike.

#### Smart Tokens (CIP-143 ProgrammableLogicGlobal)

| Scenario | Plutarch CPU | Plutarch Mem | Plinth CPU | Plinth Mem | CPU ratio | Mem ratio |
|---|---:|---:|---:|---:|---:|---:|
| seize: 1 input, complete indices | 63,419,946 | 183,327 | 67,614,449 | 206,555 | 1.07x | 1.13x |
| seize: 3 inputs, complete indices | 118,475,280 | 314,359 | 124,217,603 | 356,633 | 1.05x | 1.13x |
| seize: 5 inputs, complete indices | 173,530,614 | 445,391 | 180,724,757 | 506,111 | 1.04x | 1.14x |
| seize: 10 inputs, complete indices | 306,205,003 | 752,741 | 321,935,067 | 879,690 | 1.05x | 1.17x |
| seize: burn offsets delta | 56,695,996 | 168,717 | 63,678,755 | 192,801 | 1.12x | 1.14x |
| seize: mint contained | 74,633,703 | 214,137 | 79,615,104 | 235,009 | 1.07x | 1.10x |
| transfer: burn with mint proof | 64,551,650 | 190,015 | 74,905,237 | 235,436 | 1.16x | 1.24x |
| transfer: mint with proof+containment | 74,978,711 | 220,177 | 86,435,015 | 261,008 | 1.15x | 1.19x |

#### Guardrail (Constitution)

| Scenario | Plutarch CPU | Plutarch Mem | Plinth CPU | Plinth Mem | CPU ratio | Mem ratio |
|---|---:|---:|---:|---:|---:|---:|
| pos1: multi int params | 56,175,867 | 197,580 | 61,459,879 | 224,790 | 1.09x | 1.14x |
| pos2: single rational | 30,217,303 | 100,948 | 32,850,499 | 117,188 | 1.09x | 1.16x |
| pos3: list param | 54,843,709 | 186,140 | 60,988,771 | 221,604 | 1.11x | 1.19x |
| pos4: empty params | 5,094,145 | 17,573 | 5,233,151 | 19,241 | 1.03x | 1.09x |
| pos5: treasury withdrawal | 4,383,006 | 15,015 | 4,959,103 | 18,615 | 1.13x | 1.24x |

#### Crowdfund

| Scenario | Plutarch CPU | Plutarch Mem | Plinth CPU | Plinth Mem | CPU ratio | Mem ratio |
|---|---:|---:|---:|---:|---:|---:|
| donate: new donor | 75,126,464 | 204,741 | 62,799,985 | 195,231 | 0.84x | 0.95x |
| donate: existing donor | 79,649,731 | 215,429 | 68,100,287 | 204,556 | 0.85x | 0.95x |
| donate: second donor | 83,804,024 | 224,715 | 73,573,611 | 215,877 | 0.88x | 0.96x |
| withdraw: valid | 39,262,728 | 115,627 | 36,906,989 | 116,644 | 0.94x | 1.01x |
| reclaim: multiple donors | 100,833,696 | 266,634 | 65,427,192 | 200,981 | 0.65x | 0.75x |
| reclaim: last donor | 69,095,835 | 184,871 | 39,138,240 | 123,067 | 0.57x | 0.67x |

#### Vesting

| Scenario | Plutarch CPU | Plutarch Mem | Plinth CPU | Plinth Mem | CPU ratio | Mem ratio |
|---|---:|---:|---:|---:|---:|---:|
| full withdrawal after vesting | 38,909,388 | 112,264 | 35,340,687 | 101,467 | 0.91x | 0.90x |
| partial withdrawal midpoint | 66,088,455 | 183,674 | 56,291,830 | 157,014 | 0.85x | 0.85x |
| second partial withdrawal | 66,088,455 | 183,674 | 56,291,830 | 157,014 | 0.85x | 0.85x |
| full withdrawal at end | 39,425,890 | 113,469 | 35,857,189 | 102,672 | 0.91x | 0.90x |
| small withdrawal early | 66,088,455 | 183,674 | 56,291,830 | 157,014 | 0.85x | 0.85x |
| third partial drains | 38,909,388 | 112,264 | 35,340,687 | 101,467 | 0.91x | 0.90x |
| odd division partial | 66,088,455 | 183,674 | 56,291,830 | 157,014 | 0.85x | 0.85x |
| multi beneficiary outputs | 43,484,191 | 126,070 | 39,313,502 | 113,109 | 0.90x | 0.90x |
| zero fee | 38,909,388 | 112,264 | 35,340,687 | 101,467 | 0.91x | 0.90x |
| quarter vested | 66,088,455 | 183,674 | 56,291,830 | 157,014 | 0.85x | 0.85x |
| ninety percent | 66,088,455 | 183,674 | 56,291,830 | 157,014 | 0.85x | 0.85x |

#### SundaeSwap NFT (Settings)

| Scenario | Plutarch CPU | Plutarch Mem | Plinth CPU | Plinth Mem | CPU ratio | Mem ratio |
|---|---:|---:|---:|---:|---:|---:|
| settings admin: update scoopers | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update base fee | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update simple fee | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update strategy fee | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update pool creation fee | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update metadata admin | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update settings admin | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update treasury admin | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update extensions | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: update multiple fields | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: no change | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| settings admin: lovelace change | 56,407,319 | 138,607 | 57,207,215 | 163,440 | 1.01x | 1.18x |
| treasury admin: update treasury addr | 67,885,696 | 157,223 | 66,407,544 | 177,654 | 0.98x | 1.13x |
| treasury admin: update staking keys | 67,885,696 | 157,223 | 66,407,544 | 177,654 | 0.98x | 1.13x |
| treasury admin: update allowance | 67,885,696 | 157,223 | 66,407,544 | 177,654 | 0.98x | 1.13x |
| treasury admin: update multiple fields | 67,885,696 | 157,223 | 66,407,544 | 177,654 | 0.98x | 1.13x |
| treasury admin: no change | 67,885,696 | 157,223 | 66,407,544 | 177,654 | 0.98x | 1.13x |
| multisig: allof valid | 63,442,939 | 165,487 | 68,902,514 | 204,048 | 1.09x | 1.23x |
| multisig: anyof valid | 62,700,268 | 162,559 | 66,148,660 | 195,252 | 1.05x | 1.20x |
| multisig: atleast valid | 69,480,618 | 187,743 | 79,763,884 | 239,430 | 1.15x | 1.28x |
| multisig: before valid | 65,619,468 | 170,784 | 68,493,405 | 207,250 | 1.04x | 1.21x |
| multisig: after valid | 65,808,734 | 171,654 | 68,987,053 | 209,222 | 1.05x | 1.22x |
| mint: valid | 19,695,803 | 60,808 | 25,208,642 | 84,816 | 1.28x | 1.39x |

#### Certifying

| Scenario | Plutarch CPU | Plutarch Mem | Plinth CPU | Plinth Mem | CPU ratio | Mem ratio |
|---|---:|---:|---:|---:|---:|---:|
| register credential | 3,281,634 | 11,753 | 3,235,577 | 11,084 | 0.99x | 0.94x |
| unregister after expiration | 7,886,843 | 24,094 | 8,633,363 | 26,629 | 1.09x | 1.11x |
| delegate to abstain | 5,907,816 | 18,612 | 5,528,026 | 17,948 | 0.94x | 0.96x |
| register+delegate to abstain | 6,386,093 | 19,946 | 5,832,408 | 19,050 | 0.91x | 0.96x |

#### Voting

| Scenario | Plutarch CPU | Plutarch Mem | Plinth CPU | Plinth Mem | CPU ratio | Mem ratio |
|---|---:|---:|---:|---:|---:|---:|
| nft in script input | 12,244,560 | 30,755 | 11,461,062 | 27,727 | 0.94x | 0.90x |
| nft in pubkey input | 12,244,560 | 30,755 | 11,461,062 | 27,727 | 0.94x | 0.90x |
| nft among multiple inputs | 17,129,982 | 44,166 | 15,661,615 | 37,374 | 0.91x | 0.85x |
| nft with other tokens | 12,244,560 | 30,755 | 11,461,062 | 27,727 | 0.94x | 0.90x |
