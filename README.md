# hideshow-cycle

Cycle code folding with hideshow, progressively revealing deeper levels.

## Description

This package provides commands to cycle through code folding levels both
locally (current block) and globally (entire buffer). Each invocation
reveals one more nested level up to a configurable maximum depth. Once the
maximum is reached, the next invocation fully expands the block/buffer. If
fully visible, the next invocation hides it entirely.

Inspired by karthink's blog post "Simple folding with Hideshow":
https://karthinks.com/software/simple-folding-with-hideshow/

## Installation

```elisp
(use-package hideshow-cycle
  ;; Load from a local copy
  :load-path "/path/to/hideshow-cycle.el"
  ;; ... or clone from GitHub
  ;; :vc (:url "https://github.com/sonofjon/hideshow-cycle.el"
  ;;          :rev :newest)
  :bind (:map hs-minor-mode-map
              ("TAB" . hs-cycle)
              ("<backtab>" . hs-cycle-global)))
```

## Usage

### Local cycling

- `M-x hs-cycle` — Cycle folding levels for the current block
  - No prefix: Progressive reveal (hide → level 1 → level 2 → ... →
    show all)
  - `C-u`: Reverse direction
  - `C-u C-u`: Toggle between fully hidden and fully shown

### Global cycling

- `M-x hs-cycle-global` — Cycle folding levels for the entire buffer
  - Same prefix argument behavior as `hs-cycle`

## Configuration

The package provides one customizable variable:

- `hs-cycle-max-depth` (default: 3)
  Maximum depth level to reveal. Set to `nil` for unlimited depth.

Example:

```elisp
(setq hs-cycle-max-depth 5)
```

## Requirements

- Emacs 29.1 or newer
