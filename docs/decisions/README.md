
<!-- README.md is generated from README.Rmd. Do not edit the .md. -->

# Architecture Decision Records

Short, dated, numbered records of the decisions that shape `tccquickr`.
Each ADR states the decision, the reasoning, what it rules out, and the
consequences. They supersede prose in `fresh-compiler-redesign.md` where
the two disagree.

Rules:

- One decision per file. Number monotonically.
- State the decision in the first paragraph. No suspense.
- Record what the decision *rejects*, not only what it chooses.
- When a decision is reversed, add a new ADR that supersedes it; do not
  edit history in place.
- ADRs are authored as `.Rmd` and generated to `.md`
  (`tools/render_docs.R`).

## Index

- [0001 — Project identity: AOT optimizing
  transpiler](0001-project-identity.md)
- [0002 — Insert a lowered IR; the C target becomes a
  printer](0002-lowered-ir-seam.md)
- [0003 — Target and backend
  roadmap](0003-target-and-backend-roadmap.md)
- [0004 — Recon and JIT cleanup](0004-recon-and-jit-cleanup.md)
- [0005 — Conformance against R, Lean for transformation
  soundness](0005-conformance-and-verification.md)
