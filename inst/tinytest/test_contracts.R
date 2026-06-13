library(tinytest)
library(tccquickr)

program <- tccq_program(
  "identity",
  formals = list(x = tccq_binding("x", tccq_type("double", tccq_shape("n"))))
)

pass <- tccq_pass("identity", function(program) program)
out <- tccq_run_pipeline(program, list(pass))

expect_true(S7::S7_inherits(pass, TccqPassSpec))
expect_true(S7::S7_inherits(out, TccqProgram))
expect_equal(out@name, "identity")

bad <- tccq_pass("bad", function(program) "not a program")
err <- tryCatch(
  tccq_run_pipeline(program, list(bad)),
  error = identity
)
expect_true(inherits(err, "error"))
