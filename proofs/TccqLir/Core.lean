/-
TccqLir.Core — a denotational core for the LIR value fragment.

This mirrors the value nodes of `R/tccq_lir.R` (const / temp / binop) for the
integer element types (`int`, `xlen`). It exists so that middle-end rewrites can
be stated and proved meaning-preserving, per docs/decisions/0005-conformance-and-verification.md.

Scope is deliberately the affine/elementwise integer fragment. `double` (IEEE
floats) and memory/control nodes are added later as the LIR surface stabilizes;
this file is the seed, not the whole semantics.
-/

namespace TccqLir

/-- Binary operators of the LIR `binop` node, integer fragment. -/
inductive Op where
  | add
  | sub
  | mul
  deriving DecidableEq, Repr

/-- The LIR value fragment: a constant, a named temp/param, or a binop.
    Corresponds to `tccq_lir_const`, `tccq_lir_temp`, `tccq_lir_binop`. -/
inductive Expr where
  | const : Int → Expr
  | var   : String → Expr
  | binop : Op → Expr → Expr → Expr
  deriving Repr

/-- Apply an operator. -/
def Op.apply : Op → Int → Int → Int
  | .add, a, b => a + b
  | .sub, a, b => a - b
  | .mul, a, b => a * b

/-- Denotation: the meaning of an expression under an environment binding the
    free temps/params. This is the ground truth a pass must preserve. -/
def denote (env : String → Int) : Expr → Int
  | .const n      => n
  | .var x        => env x
  | .binop o a b  => o.apply (denote env a) (denote env b)

/-- Constant folding: a binop whose operands fold to constants becomes a
    constant. This is the rewrite that `tccq_jit` constant specialization and
    the fusion/peephole passes will perform on the real LIR
    (docs/decisions/0004-recon-and-jit-cleanup.md). -/
def fold : Expr → Expr
  | .const n     => .const n
  | .var x       => .var x
  | .binop o a b =>
    match fold a, fold b with
    | .const x, .const y => .const (o.apply x y)
    | a',       b'       => .binop o a' b'

/-- Soundness: constant folding preserves denotation under every environment.
    This is the shape every pass-correctness theorem in this project takes. -/
theorem fold_preserves (env : String → Int) :
    ∀ e : Expr, denote env (fold e) = denote env e := by
  intro e
  induction e with
  | const n => rfl
  | var x => rfl
  | binop o a b iha ihb =>
    -- Restate the target denotation in terms of the *folded* operands using the
    -- induction hypotheses, so both sides speak about `fold a` / `fold b`.
    have rhs : denote env (Expr.binop o a b)
        = o.apply (denote env (fold a)) (denote env (fold b)) := by
      rw [denote, ← iha, ← ihb]
    rw [rhs]
    -- Now case-split the folded operands; the match collapses and each arm is rfl.
    simp only [fold]
    cases fold a <;> cases fold b <;> rfl

end TccqLir
