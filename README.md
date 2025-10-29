# hideshow-cycle.el

Cycle code folding with hideshow, progressively revealing deeper levels.

## Overview

This package provides commands to cycle through code folding levels both
locally (current block) and globally (entire buffer). Each invocation reveals
one more nested level up to a configurable maximum depth. Once the maximum
is reached, the next invocation fully expands the block/buffer. If fully
visible, the next invocation hides it entirely.

## Inspiration

Inspired by karthink's hideshow utilities:
https://karthinks.com/software/simple-folding-with-hideshow/

## Features

- **Local cycling** (`hs-cycle`): Cycle folding for the current code
  block
- **Global cycling** (`hs-cycle-global`): Cycle folding for the entire
  buffer
- **Prefix arguments**: Reverse direction or toggle between fully
  hidden/shown
- **Configurable depth**: Set maximum folding depth with
  `hs-cycle-max-depth`

## Installation

Add to your `package-vc-selected-packages` list:

```elisp
(setq package-vc-selected-packages
      '((hideshow-cycle :url
         "https://github.com/sonofjon/hideshow-cycle.el"
         :rev :newest)))
```

Then configure in your Emacs config:

```elisp
(use-package hideshow-cycle
  :custom
  (hs-cycle-max-depth 3)
  :bind
  (:map hs-minor-mode-map
        ("TAB" . hs-cycle)
        ("<backtab>" . hs-cycle-global)))
```

## Usage

### Basic Commands

- `hs-cycle`: Cycle folding levels for the current block
  - No prefix: Progressive reveal (hide → level 1 → level 2 → ... →
    show all)
  - `C-u`: Reverse direction
  - `C-u C-u`: Toggle between fully hidden and fully shown

- `hs-cycle-global`: Cycle folding levels globally
  - Same prefix argument behavior as `hs-cycle`

### Customization

```elisp
;; Set maximum depth level (default: 3)
(setq hs-cycle-max-depth 5)

;; Set to nil for unlimited depth
(setq hs-cycle-max-depth nil)
```

## License

Free and unencumbered software released into the public domain.
