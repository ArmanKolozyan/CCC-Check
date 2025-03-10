# Evaluating the Effectiveness of Value Inferencing in PICUS


## Context

PICUS ([GitHub](https://github.com/Veridise/Picus), [Paper](https://dl.acm.org/doi/10.1145/3591282)) is a tool that combines SMT solving with static analysis to detect underconstrained bugs in R1CS. The role of the static analysis component is to reduce the number of timeouts in SMT solving, as solving constraints over large prime fields can be computationally expensive.

Static analysis contributes to improving SMT solving efficiency in two ways:

1. Uniqueness propagation: If a variable is guaranteed to have a unique value, this information is added as a constraint.
2. Value inferencing: If a variable is constrained to a certain range of values, this information is added as a constraint.

## Experiment

While the paper evaluates how SMT solving performs without static analysis (see Figure 11 in the paper), it is unclear how effective the value inferencing phase (point 2 above) is and much it contributes to reducing timeouts. 

To investigate this, we conducted an experiment to answer the following questions:
1. How many variables are successfully restricted to a certain range of values?
2. To what ranges are they restricted?

To systematically evaluate the impact of value inferencing, we wrote a Python script (`Analysis/ranges_collector.py`) to run PICUS on all its benchmark files and collect range constraints for each variable.  A timeout of 3 minutes per benchmark was imposed. We then wrote a data analysis script (`Analysis/ranges_analyzer.ipynb`) to compute statistics on the results.

Below is a summary of the main results:

| Metric | Value |
|--------|-------|
| **Total benchmarks analyzed** | 559 |
| **Total restricted variables** | 152,776 / 4,504,380 |
| **Percentage of restricted variables** | 3.39% |
| **Average percentage of restricted variables per benchmark** | 18.43% |
| **Total number of timeouts** | 142 / 559 |

We can conclude from this that the vast majority of variables seem to remain unconstrained. Moreover, the results clearly show that all constrained variables were restricted to the `{0,1}` range. This indicates that only binary constraints are being propagated. The low percentage of restricted variables (3.39%) suggests that the value inference mechanisms in PICUS are either not fully implemented and/or are not fully effective.

## Problems

After looking deeper into the code and the results, it seems that the value inferencing in PICUS is limited in scope and does not fully implement all possible inference rules described in the paper. The main issues identified are:

- **`binary01-lemma`** identifies variables that must be either 0 or 1 by detecting constraints of the form $x * (x - 1) = 0$. This equation enforces that `x` can only take values from {0,1} because any other value would result in a nonzero product. The lemma is only applied when the possible values are explicitly {0,1}. Other similar constraints, such as $(x-a)(x-b) = 0$ for arbitrary $a, b$, are not handled.

- **`aboz-lemma`** From a high-level perspective, the “all-but-one-zero” (ABOZ) lemma says that if you have a collection of variables $y_0,...,y_n$ and a single “selector” x such that
    1. For each $i$, $y_i*(x−i)=0$
    2. $y_0+...+y_n$ = some constant c

    then exactly one of $y_0,...,y_n$ must be `c` and all the others must be zero, provided `x` is already known (unique) to be one of the integers $0,...,n$. The version in PICUS supports only a a two-value (`n=1`) special case of ABOZ.

- **`basis2-lemma`** identifies cases where a variable is expressed as a sum of weighted powers of 2. The constraints have the form $z = 2^0 x_0 + 2^1 x_1 + \dots + 2^n x_n$ where $x_i$ are binary variables. The lemma only handles strict powers-of-two expansions, it does not generalize to arbitrary bases.

## Proposed Improvements

To improve the value inferencing, we can take the following steps:

1. Implementing a more general and complete version of the inference rules described in the paper, as discussed above.

2. Adding more general pattern-matching techniques to capture value constraints in a wider variety of cases beyond the ones mentioned in the paper.

## Examples

In this section, we present examples to demonstrate the benefits of a more general value inferencer.

### simpleMult

The `simpleMult.circom` example illustrates another limitation in PICUS's value inference. The circuit defines the following constraint:

`in * (in - 2) === 0`

Mathematically, this enforces that `in` must be either `0` or `2`. However, PICUS does not successfully restrict `in` to these values; it remains unconstrained in the analysis.
When writing sing the constraint:

`in * (in - 1) === 0`

PICUS does successfully infer that `in` must be either `0` or `1`. This illustrates that the `binary01-lemma` is specifically designed to detect only `{0,1}` patterns. Moreover, no ranges are inferred when flipping the constraint:

`0 === in * (in - 1);`

Despite being mathematically equivalent, PICUS does not infer that `in` is binary in this case. This suggests that its pattern-matching mechanism is sensitive to the order of terms rather than the underlying mathematical structure.

### toBinary

Consider the `Examples/toBinary.circom` file, which defines a circuit that sets `out` to 1 if `in` is nonzero and 0 otherwise. Mathematically, `out` can only be 0 or 1:

- If `in = 0`, the product `in*(1 - out)` must be 0, forcing `out` to 0.
- If `in ≠ 0`, then `inv = 1/in`, so `out = in*inv = 1`.

However, since the existing lemmas in PICUS do not match these constraints directly, PICUS fails to recognize that `out ∈ {0,1}`. To detect this automatically, we can introduce a rule for constraints of the form `a*(1 - b) === 0`, combined with `b <== a*c`. If we see `b = a * something` and simultaneously `a*(1 - b) = 0`, we conclude that if `a=0` then `b=0`, and if `a≠0` then `b=1`.

### wrongOr

The `WrongOr.circom` example uses the `OR` gate from Circomlib but does not explicitly constrain its inputs to be binary. The `OR` gate in Circomlib is implemented as:

`out <== a + b - a * b;`

This assumes that `a` and `b` are binary. However, if we input `a = 2` and `b = 2`, we expect `result = 1`, but the formula actually computes `2 + 2 - (2 * 2) = 0`, which is not what we expect. A naive fix would be to add constraints in the OR gate enforcing that `a` and `b` are binary. However, this would significantly increase the number of constraints, which would make proof generation slower. To address this, Circom introduced Signal Tagging, where variables can be annotated as `{binary}` to indicate they should only take values in `{0,1}`. However, these tags are not actually enforced at compile-time, which means incorrect values can still propagate through the circuit. With value inferencing, we can statically verify whether variables marked as `{binary}` are truly constrained to binary values.

### overflow

Sources: https://hackmd.io/@blockdev/Bk_-jRkXa, https://github.com/0xPARC/zk-bug-tracker?tab=readme-ov-file#3-arithmetic-overunder-flows, https://github.com/0xPARC/zk-bug-tracker?tab=readme-ov-file#4-mismatching-bit-lengths, https://gist.github.com/LCamel/4638804256815beb78e672b3716d0626

extra: https://www.rareskills.io/post/circom-aliascheck




## TO DO

### overflow

* bug uitleggen
* TO THINK: hun verschil moet in n bits kunnen voorgesteld worden, kunnen we geen overflow bereiken zonder iets met p te doen?

### Research Objectives

* sectie schrijven
* verband met https://github.com/0xPARC/zk-bug-tracker?tab=readme-ov-file#4-mismatching-bit-lengths
* verband met signal tagging => discrepancies
* verband met SMT solving
* verband met CirC

