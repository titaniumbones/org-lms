#!/bin/zsh
# Batch-run ERT tests for org-grading.
# The repo MUST come first on load-path: ~/.config/emacs/elpaca/builds/org-lms
# is a separate checkout with a stale ox-canvashtml.elc that would shadow it.
cd /Users/pricemat/src/org-grading || exit 1
LP=(-L /Users/pricemat/src/org-grading -L /Users/pricemat/src/org-grading/test)
for d in ~/.config/emacs/elpaca/builds/*(/); do
  case "$d" in (*/org-lms/|*/org-lms-old/) continue;; esac
  LP+=(-L "$d")
done
exec emacs --batch -Q "${LP[@]}" "$@"
