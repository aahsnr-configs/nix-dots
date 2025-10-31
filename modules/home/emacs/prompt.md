Using the detailed LaTeX writing environment from the attached `emacs.txt` file as a blueprint for desired features, create a comprehensive LaTeX configuration for Doom Emacs. This new configuration should be added as `emacs-lisp` blocks within the `doom.txt` file, which is structured as a literate `config.org`.

### General Instructions and Methodology

1.  **Preliminary Research (Crucial First Step):** Before writing any code for the sections below, you must **search the web** to verify the current default behaviors of Doom Emacs's `lang/latex` module and any related packages (like `lsp-bridge`, `citar`, etc.). The primary goal is to confirm that a requested feature is not already provided by default. This prevents redundant configuration and ensures the final output is clean and idiomatic.

2.  **Doom Emacs Syntax and Best Practices:** All configurations must adhere strictly to Doom Emacs conventions. This includes using Doom's macros like `use-package!`, `after!`, `add-hook!`, and `map!`. Avoid using standard Emacs Lisp functions where a Doom equivalent exists to ensure the configuration is idiomatic and maintainable.

### Configuration Requirements

Your configuration must integrate the following specific tools and customizations, guided by your preliminary research to add them *only* if they are not already handled by Doom's default setup:

**1. LaTeX Engine:**
    -   Configure **AUCTeX** to use **Tectonic** as the default `TeX-engine`. Use the setup guide at [https://tectonic-typesetting.github.io/book/latest/howto/auctex-setup/index.html](https://tectonic-typesetting.github.io/book/latest/howto/auctex-setup/index.html) as the primary reference for this integration.

**2. LSP and Diagnostics:**
    -   Ensure that **texlab** is configured as the LSP server for LaTeX. Note that I am using `lsp-bridge` (as configured in `doom.txt`), so the integration should be compatible with it.
    -   Adhere to the instruction that `apheleia` (formatting) and `flymake` (diagnostics) do not need separate configuration for LaTeX, as these functionalities are expected to be handled by `texlab` and `lsp-bridge`.

**3. Citation Management:**
    -   Set up the **Citar** ecosystem for citations. While Doom's `+citar` flag provides the base packages, you must add the following user-specific customizations:
        -   Set the `citar-bibliography`, `citar-library-paths`, and `citar-notes-paths` variables to placeholder paths (e.g., `~/path/to/references.bib`, `~/Zotero/storage`, `~/org-roam/`).
        -   Configure `citar-symbols` to use `nerd-icons`.
    -   Ensure `RefTeX` is configured to *only* handle non-citation references, letting Citar manage the bibliography.

**4. Editing and UI Enhancements:**
    -   Integrate **`laas`** (Live Auto-Activating Snippets) and add the custom snippets for Greek letters, math operators, and quick environments as defined in `emacs.txt`.
    -   Customize Doom's default `prettify-symbols-mode` configuration by replacing or extending its symbol list (`+latex-prettify-symbols-alist`) with the specific, more comprehensive set provided in `emacs.txt`.

**5. Org Mode Integration:**
    -   Configure Org mode's LaTeX exporter (`ox-latex`) to use **Tectonic** as the compiler.
    -   Add the custom `org-latex-classes` for "article" and "beamer" to the configuration.

**6. Custom Snippets:**
    -   Add the custom **`yasnippet`** snippets for `latex-mode` from `emacs.txt`. These snippets, tailored for scientific writing (e.g., auto-labeled equations, physics notations), should supplement or override the default snippet collection.

For context, the default Doom Emacs `lang/latex` module files are located here:
- `config.el`: [https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/config.el](https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/config.el)
- `autoload.el`: [https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/autoload.el](https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/autoload.el)
- `+fontification.el`: [https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/%2Bfontification.el](https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/%2Bfontification.el)
- `+ref.el`: [https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/%2Bref.el](https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/%2Bref.el)
- `+viewers.el`: [https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/%2Bviewers.el](https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/%2Bviewers.el)

The final output should consist of well-structured `emacs-lisp` source blocks, suitable for inclusion in a Doom Emacs `config.org` file, with brief comments explaining each section's purpose.
