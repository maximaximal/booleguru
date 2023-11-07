-- A small utility to extract prefix data from a QBF. Inspired from
-- https://gitlab.sai.jku.at/qbf/software/prefixtract
function prefixtract(op)
  alternations = 0
  vars_exists = 0
  vars_forall = 0

  last_t = optype.none
  while op.t == optype.exists or op.t == optype.forall do
    if last_t ~= op.t then
      alternations = alternations + 1
    end

    if op.t == optype.exists then
      vars_exists = vars_exists + 1
    end
    if op.t == optype.forall then
      vars_forall = vars_forall + 1
    end

    last_t = op.t
    op = op.r
  end

  print("QAs: " .. alternations .. " Existential: " .. vars_exists .. " Universal: " .. vars_forall .. " Vars-in-Prefix: " .. (vars_exists + vars_forall))
end
