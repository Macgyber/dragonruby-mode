#!/bin/bash
# Test runner script

echo "=========================================="
echo "DragonRuby Mode - Test Runner"
echo "=========================================="
echo ""

cd "$(dirname "$0")"

# Run Emacs tests
echo "Running Emacs batch tests..."
emacs --batch \
  -L src \
  -L src/core \
  -L src/concepts \
  -L src/ui \
  -L src/mode \
  -L src/modules \
  --eval "(require 'dragonruby)" \
  --eval "(message \"✓ Successfully loaded dragonruby.el\")" \
  --eval "(message \"\")" \
  --eval "(message \"Checking registered concepts...\")" \
  --eval "(let ((concepts '(\"tick\" \"args\" \"args.state\" \"args.inputs\" \"args.outputs\" \"args.inputs.keyboard\" \"args.inputs.mouse\" \"args.outputs.sprites\" \"args.outputs.labels\" \"args.outputs.solids\" \"color-array\"))) (dolist (id concepts) (let ((c (dragonruby-get-concept id))) (if c (message \"  ✓ %s - %s\" id (dragonruby-concept-name c)) (message \"  ✗ %s - NOT FOUND\" id)))))" \
  --eval "(message \"\")" \
  --eval "(message \"Test complete!\")"

echo ""
echo "=========================================="
echo "Done!"
echo "=========================================="
