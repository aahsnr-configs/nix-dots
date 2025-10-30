# LSP-Bridge Quick Start Guide

# LSP-Bridge Quick Start Guide

A minimal 5-minute setup guide to get lsp-bridge working with Python in Doom Emacs.

## Prerequisites Check

```bash
# Check Emacs version (need 28+, 29+ recommended)
emacs --version

# Check Python version (need 3.8+)
python3 --version

# Check if pip is available
pip3 --version
```

## Quick Installation (5 Steps)

### Step 1: Install Python Dependencies (2 minutes)

```bash
pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging
```

### Step 2: Install Language Servers (1 minute)

```bash
# Install basedpyright
pip3 install basedpyright

# Install ruff
pip3 install ruff

# Verify installation
basedpyright-langserver --version
ruff --version
```

### Step 3: Update Doom Configuration (1 minute)

Add to `~/.doom.d/packages.el`:

```elisp
;; Disable conflicts
(package! lsp-mode :disable t)
(package! eglot :disable t)
(package! company :disable t)

;; Install lsp-bridge
(package! lsp-bridge
  :recipe (:host github
           :repo "manateelazycat/lsp-bridge"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not compile)))
```

Add to `~/.doom.d/config.el`:

```elisp
(use-package! yasnippet
  :config
  (yas-global-mode 1))

(use-package! lsp-bridge
  :config
  (global-lsp-bridge-mode)
  (setq lsp-bridge-python-multi-lsp-server "basedpyright_ruff"))
```

Update `~/.doom.d/init.el`:
- Comment out `:tools lsp`
- Comment out `:tools eglot`
- Enable `(syntax +flymake)` in `:checkers`

### Step 4: Sync Doom (1 minute)

```bash
doom sync
```

### Step 5: Restart Emacs

```bash
doom reload
# or restart Emacs
```

## Quick Test

1. Create a test Python file: `test.py`

```python
import pandas as pd

def greet(name: str) -> str:
    """Greet someone by name."""
    return f"Hello, {name}!"

# Type 'pd.' and you should see completions
df = pd.DataFrame()

# Press M-. on greet to jump to definition
result = greet("World")
print(result)
```

2. Open the file in Emacs: `emacs test.py`

3. Test these features:
   - **Completion**: Type `pd.` and press TAB
   - **Jump to definition**: Put cursor on `greet` and press `M-.`
   - **Documentation**: Put cursor on `DataFrame` and press `K`
   - **Diagnostics**: Save the file (`C-x C-s`) to see linting

## Verify Installation

Run these commands in Emacs:

```
M-x lsp-bridge-list-servers
```

You should see:
- `basedpyright` running
- `ruff` running

## Common First-Time Issues

### Issue: "No completion appearing"

**Solution:**
```elisp
M-x lsp-bridge-restart-process
```

### Issue: "basedpyright not found"

**Solution:**
```bash
which basedpyright-langserver
# If not found, reinstall:
pip3 install --force-reinstall basedpyright
```

### Issue: "Module 'epc' not found"

**Solution:**
```bash
pip3 install --upgrade epc orjson sexpdata six
```

### Issue: "Company popup showing"

**Solution:** Company is conflicting. Add to config.el:
```elisp
(after! company
  (global-company-mode -1))
```

## Essential Keybindings

| Action | Keybinding | Command |
|--------|-----------|---------|
| Complete | `TAB` | Auto-trigger |
| Jump to definition | `M-.` | `lsp-bridge-find-def` |
| Jump back | `M-,` | `lsp-bridge-find-def-return` |
| Documentation | `K` | `lsp-bridge-show-documentation` |
| Rename | `SPC c r` | `lsp-bridge-rename` |
| Code action | `SPC c a` | `lsp-bridge-code-action` |
| Format | `SPC c f` | `lsp-bridge-code-format` |

## Next Steps

Once basic setup works:

1. **Add format-on-save**: Install apheleia package
2. **Customize Ruff**: Create `pyproject.toml` in your project
3. **Configure basedpyright**: Set type checking mode in `pyproject.toml`
4. **Enable org-babel**: For literate programming support
5. **Add keybindings**: Customize to your workflow

See the comprehensive configuration artifacts for full setup.

## Minimal pyproject.toml

Create this in your project root:

```toml
[tool.ruff]
line-length = 88
target-version = "py311"

[tool.ruff.lint]
select = ["E", "F", "I", "W"]

[tool.basedpyright]
typeCheckingMode = "basic"
```

## Getting Help

If something doesn't work:

1. Check `*lsp-bridge*` buffer for errors
2. Run `M-x lsp-bridge-restart-process`
3. Enable debug: `(setq lsp-bridge-enable-log t)`
4. Check [lsp-bridge issues](https://github.com/manateelazycat/lsp-bridge/issues)

## Performance Tip

For even better performance, install `orjson` if you haven't:

```bash
pip3 install orjson
```

This provides faster JSON parsing (written in Rust).

---

**You're all set!** 🎉 You now have a blazingly fast Python development environment in Doom Emacs.

For the full configuration with all quality-of-life features, see the comprehensive config.el artifact.
