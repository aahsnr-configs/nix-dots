Please apply a series of targeted enhancements to the attached Doom Emacs configuration files (`packages.el`, `init.el`, and `config.org`). The primary objective is to create a state-of-the-art Python development environment by integrating powerful, standalone tools for formatting, linting, and debugging, while also refining the UI completion settings. Make sure to search the web and the provided links before writing anything.

**1. Refine Corfu Completion UI**

- Explicitly disable corfu-popup-info from corfu as you can see below that doom emacs automatically enables corfu popupinfo.
- Consult the official Doom Emacs `corfu` module configuration (`https://github.com/doomemacs/doomemacs/blob/master/modules/completion/corfu/config.el`) as a reference to ensure the remaining settings (`corfu-auto`, `corfu-cycle`, etc.) are optimally configured for a clean, out-of-the-box experience.

**2. Configure the Python Development Environment**

This section focuses on replacing the default, LSP-centric toolchain with a combination of specialized, high-performance tools (`basedpyright`, `ruff`, `mypy`) for a more responsive and powerful workflow.

**2.1. Language Server Protocol (LSP)**

- Explicitly configure `lsp-mode` to use `basedpyright` as the language server for Python, which offers more features than the default `lsp-pyright`. Do not use any variable associated with `lsp-pyright`

**2.2. Code Formatting (Apheleia + Ruff)**

- Decouple code formatting from the LSP. First, explicitly disable `lsp-mode` from handling formatting for Python code to prevent conflicts.
- Configure `apheleia` to use `ruff` for all Python buffer formatting using `apheleia`. For guidance on how `apheleia` is configured, refer to Doom's `format` module (`https://github.com/doomemacs/doomemacs/blob/master/modules/editor/format/config.el`).

**2.3. Syntax Checking (Flycheck + Ruff + Mypy)**

- Replace the default LSP-based syntax checking with a custom `flycheck` toolchain for superior performance and control.
- First, disable diagnostics from the LSP for Python buffers to avoid redundant error reportingo, make sure that flycheck does not use lsp checkers.
- Next, configure `flycheck` to create a checker chain for `python-mode` that first runs the fast `ruff` linter, followed by the more comprehensive `mypy` type-checker. You can find examples of setting up `flycheck` checkers in Doom's syntax module (`https://github.com/doomemacs/doomemacs/blob/master/modules/checkers/syntax/config.el`).

**2.4. Debugging (Dape for Python)**

- Set up the Debug Adapter Protocol for Python debugging.
- Refer to Doom's debugger module config for the correct structure (`https://github.com/doomemacs/doomemacs/blob/master/modules/tools/debugger/config.el`).

**3. Optimize Org Mode Jupyter Integration**

To ensure a smooth and conflict-free experience with the new LSP setup, Jupyter's code completion features within Org mode should be disabled. The goal is to use Jupyter exclusively for code block execution, kernel management that are specific to jupyter.

- In your `config.txt`, add a configuration block that modifies the Org Babel setup for Jupyter.
- Identify the mechanism that provides Jupyter completions within Org source blocks and disable it. The `jupyter.el` file from Doom's `org` module (`https://github.com/doomemacs/doomemacs/blob/master/modules/lang/org/contrib/jupyter.el`) can be used as a reference to find the correct variable.
