# Yazi Configuration User Guide

## Table of Contents

1. [What is Yazi?](#what-is-yazi)
2. [Installation](#installation)
3. [Basic Navigation](#basic-navigation)
4. [File Operations](#file-operations)
5. [Advanced Features](#advanced-features)
6. [Plugin Guide](#plugin-guide)
7. [Keybindings Reference](#keybindings-reference)
8. [Tips & Tricks](#tips--tricks)

---

## What is Yazi?

Yazi is a blazing-fast terminal file manager written in Rust. It features:

- **Async I/O** for smooth performance
- **Built-in preview** for various file types
- **Vim-like keybindings** for efficient navigation
- **Plugin system** for extensibility
- **Tab support** for multitasking

---

## Installation

### Using This Configuration

1. **Place the configuration file** in your home-manager modules:
   ```bash
   ~/nix-dots/modules/home/yazi/default.nix
   ```

2. **Import it** in your home-manager configuration:
   ```nix
   imports = [
     ./modules/home/yazi/default.nix
   ];
   ```

3. **Rebuild your system**:
   ```bash
   home-manager switch
   ```

4. **Launch Yazi**:
   ```bash
   yazi
   ```

---

## Basic Navigation

### Vim-Style Movement

| Key | Action         | Description                             |
| --- | -------------- | --------------------------------------- |
| `h` | Go back        | Exit current directory (move to parent) |
| `j` | Move down      | Select next item in the list            |
| `k` | Move up        | Select previous item in the list        |
| `l` | Smart enter    | Open file OR enter directory (smart!)   |
| `g` | Jump to top    | Go to first item                        |
| `G` | Jump to bottom | Go to last item                         |

### Quick Tips:

- Use `j`/`k` for smooth scrolling through files
- Press `l` to intelligently open files or enter directories
- Press `h` to go back to the parent directory
- The **smart-enter** plugin makes `l` context-aware!

---

## File Operations

### Copy, Cut, Paste

| Key | Action      | Description                                       |
| --- | ----------- | ------------------------------------------------- |
| `y` | Yank (copy) | Copy selected file(s)                             |
| `x` | Cut         | Cut selected file(s)                              |
| `p` | Smart paste | Paste into hovered directory or current directory |

**Smart Paste Feature**: If you hover over a directory and press `p`, files
paste INTO that directory. If hovering over a file, they paste into the current
directory.

### Creating & Removing

| Key  | Action      | Description                               |
| ---- | ----------- | ----------------------------------------- |
| `a`  | Create      | Create new file or directory              |
| `dd` | Delete      | Remove selected file(s) (press `d` twice) |
| `r`  | Rename      | Rename current file                       |
| `cm` | Change mode | Change file permissions (chmod)           |

### Selection

| Key      | Action           | Description                           |
| -------- | ---------------- | ------------------------------------- |
| `Space`  | Toggle selection | Select/deselect current file          |
| `v`      | Visual mode      | Enter visual selection mode           |
| `V`      | Select all       | Select all files in current directory |
| `Ctrl-a` | Select all       | Alternative method                    |

---

## Advanced Features

### Tab Management

| Key   | Action       | Description                           |
| ----- | ------------ | ------------------------------------- |
| `t`   | New tab      | Create new tab with current directory |
| `w`   | Close tab    | Close current tab                     |
| `[`   | Previous tab | Switch to previous tab                |
| `]`   | Next tab     | Switch to next tab                    |
| `1-5` | Jump to tab  | Directly switch to tab 1-5            |

**Use Case**: Open multiple directories simultaneously for easy file comparison
or organization.

### Pane Toggling

| Key  | Action           | Description                           |
| ---- | ---------------- | ------------------------------------- |
| `T`  | Toggle preview   | Show/hide the preview pane            |
| `zp` | Toggle parent    | Show/hide the parent directory pane   |
| `zm` | Maximize preview | Maximize the preview pane temporarily |

**Tip**: Use `T` to get more space when you don't need previews, or `zm` to
examine a file in detail.

### Search & Filter

| Key | Action         | Description                                 |
| --- | -------------- | ------------------------------------------- |
| `/` | Smart filter   | Filter files interactively (starts typing)  |
| `s` | Search with fd | Find files by name using fd                 |
| `S` | Search with rg | Search file contents using ripgrep          |
| `z` | Zoxide jump    | Quickly jump to frequently used directories |
| `f` | Jump to char   | Vim-like character jump in filename list    |

**Smart Filter**: As you type, the list updates in real-time. Press `Enter` to
select the first match, `Esc` to cancel.

**Jump to Char**: Type `f` followed by a character to quickly jump to the next
file starting with that character.

### Bookmarks

| Key | Action           | Description                                 |
| --- | ---------------- | ------------------------------------------- |
| `m` | Save bookmark    | Bookmark current directory                  |
| `'` | Jump to bookmark | Open fuzzy finder to jump to saved bookmark |

**Use Case**: Save frequently accessed directories like `~/Documents`,
`~/Projects`, etc. Press `'` for instant access!

---

## Plugin Guide

### 1. **Full Border**

- **What it does**: Adds a clean rounded border around Yazi
- **Activation**: Automatic on startup
- **Benefit**: Better visual separation, more polished look

### 2. **Smart Enter**

- **What it does**: Makes `l` key intelligent
  - On files: Opens them
  - On directories: Enters them
- **Keybinding**: `l`
- **Why it's useful**: No need to remember different keys for files vs
  directories

### 3. **Jump to Char**

- **What it does**: Vim-style character jumping
- **Keybinding**: `f` + character
- **Example**: In a list of files, press `fd` to jump to the next file starting
  with "d"
- **Why it's useful**: Navigate long file lists instantly

### 4. **Smart Paste**

- **What it does**: Context-aware pasting
  - Hover directory + `p` = paste INTO directory
  - Hover file + `p` = paste into current directory
- **Keybinding**: `p`
- **Why it's useful**: Move files to subdirectories without entering them

### 5. **Toggle Pane**

- **What it does**: Show/hide different panes
- **Keybindings**:
  - `T` → Toggle preview pane
  - `zp` → Toggle parent directory pane
  - `zm` → Maximize preview
- **Why it's useful**: Customize your workspace on the fly

### 6. **Chmod**

- **What it does**: Interactive permission editor
- **Keybinding**: `cm`
- **Why it's useful**: Change file permissions without leaving Yazi

### 7. **Git Integration**

- **What it does**: Shows git status in file listings
- **Keybindings**:
  - `gg` → Open Lazygit in current directory
  - `gs` → Show git status
- **Why it's useful**: Visual git status, quick access to Lazygit for
  commits/pushes

### 8. **Lazygit**

- **What it does**: Full git interface within Yazi
- **Keybinding**: `gg`
- **Why it's useful**: Stage, commit, push, pull without leaving Yazi

### 9. **Rich Preview**

- **What it does**: Enhanced preview for:
  - Markdown (`.md`)
  - JSON (`.json`)
  - CSV (`.csv`)
  - Jupyter notebooks (`.ipynb`)
  - reStructuredText (`.rst`)
- **Activation**: Automatic when viewing supported files
- **Why it's useful**: Beautifully formatted previews with syntax highlighting

### 10. **Ouch**

- **What it does**: Archive preview and extraction
- **Supported formats**: ZIP, TAR, 7Z, RAR, XZ, ZSTD, BZIP2
- **Usage**:
  - Preview: Automatic when hovering over archives
  - Extract: Select archive → Press `Enter` → Choose "Extract with ouch"
- **Why it's useful**: See archive contents without extracting

### 11. **Yatline**

- **What it does**: Beautiful status and header lines
- **Features**:
  - Shows date/time
  - File information
  - Tab indicators
  - Permission colors
  - Selection count
- **Activation**: Automatic
- **Why it's useful**: Always know where you are and what you're doing

### 12. **Starship**

- **What it does**: Integrates Starship prompt
- **Why it's useful**: Consistent prompt style across shell and file manager

### 13. **YAMB (Bookmarks)**

- **What it does**: Persistent bookmark manager
- **Keybindings**:
  - `m` → Save current directory
  - `'` → Fuzzy search and jump to bookmark
- **Why it's useful**: Instant access to your most-used directories

### 14. **Diff**

- **What it does**: Compare two files visually
- **Keybinding**: `=`
- **Usage**: Select two files, press `=`
- **Why it's useful**: Quick file comparison without external tools

### 15. **Smart Filter**

- **What it does**: Real-time file filtering
- **Keybinding**: `/`
- **Features**:
  - Live filtering as you type
  - Auto-enter on single match
  - Fuzzy matching
- **Why it's useful**: Find files instantly in large directories

---

## Keybindings Reference

### Complete Keybinding List

#### Navigation

```
h         → Go back (parent directory)
j         → Move down
k         → Move up
l         → Smart enter (open/enter)
g         → Jump to top
G         → Jump to bottom
f         → Jump to char
z         → Zoxide jump
```

#### File Operations

```
y         → Copy (yank)
x         → Cut
p         → Smart paste
dd        → Delete
a         → Create new file/directory
r         → Rename
cm        → Change permissions (chmod)
Space     → Toggle selection
v         → Visual mode
V         → Select all
```

#### Search & Filter

```
/         → Smart filter
s         → Search with fd
S         → Search with ripgrep (in contents)
```

#### Tabs

```
t         → New tab
w         → Close tab
[         → Previous tab
]         → Next tab
1-5       → Jump to specific tab
```

#### Panes

```
T         → Toggle preview pane
zp        → Toggle parent pane
zm        → Maximize preview
```

#### Git

```
gg        → Open Lazygit
gs        → Show git status
```

#### Bookmarks

```
m         → Save bookmark
'         → Jump to bookmark
```

#### Miscellaneous

```
:         → Shell command (interactive)
!         → Shell command (blocking)
?         → Help menu
Ctrl-h    → Toggle hidden files
Ctrl-z    → Suspend Yazi
=         → Diff files
q         → Quit
```

---

## Tips & Tricks

### 1. **Quick Directory Navigation**

```bash
# Bookmark your most used directories first
cd ~/Projects && yazi  # Press 'm' to save
cd ~/Documents && yazi # Press 'm' to save
cd ~/Downloads && yazi # Press 'm' to save

# Now use ' (apostrophe) anywhere to instantly jump!
```

### 2. **Bulk File Operations**

```
1. Press 'v' to enter visual mode
2. Use 'j/k' to select multiple files
3. Press 'y' to copy or 'x' to cut
4. Navigate to destination
5. Press 'p' to paste
```

### 3. **Preview Large Files**

```
- Hover over file
- Press 'zm' to maximize preview
- Read comfortably
- Press 'zm' again to restore
```

### 4. **Working with Archives**

```
- Hover over .zip/.tar/.7z file
- Preview shows contents automatically
- Press 'l' to see extraction options
- Choose "Extract with ouch"
```

### 5. **Git Workflow**

```
1. Navigate to git repository
2. See file statuses automatically (git plugin)
3. Press 'gg' to open Lazygit
4. Stage, commit, push all in one place
5. Press 'q' to return to Yazi
```

### 6. **Filter vs Search**

```
/ (Filter)  → Shows only matching files in current directory
s (fd)      → Recursively searches all subdirectories by name
S (rg)      → Searches file CONTENTS recursively
```

### 7. **Smart Paste Workflow**

```
# Traditional way:
1. Copy files
2. Enter target directory
3. Paste

# Smart paste way:
1. Copy files
2. Just hover over target directory
3. Press 'p' → files go directly into it!
```

### 8. **Tab Workflow**

```
# Organize projects across tabs:
Tab 1 → ~/Projects/project-a
Tab 2 → ~/Documents/research
Tab 3 → ~/Downloads
Tab 4 → ~/Pictures

# Switch instantly with [ ] or 1-5 keys
```

### 9. **Hidden Files**

```
Ctrl-h → Toggle hidden file visibility
Useful for: .git, .env, .config files
```

### 10. **Shell Integration**

```
# Run quick commands without leaving Yazi:
:git status              # Interactive shell
!ls -la                  # Blocking shell (waits for completion)
```

---

## Common Workflows

### Workflow 1: Code Project Management

```
1. Open project directory in Yazi
2. Press 'gg' to check git status with Lazygit
3. Use 'cm' to fix permissions on scripts
4. Press 'l' to open files in Neovim
5. Use tabs to manage multiple project directories
```

### Workflow 2: Media Organization

```
1. Navigate to ~/Downloads
2. Press 'v' to select photos/videos
3. Press 'y' to copy
4. Navigate to ~/Pictures/2024
5. Hover over specific album folder
6. Press 'p' to paste directly into album
```

### Workflow 3: Document Research

```
1. Press 'm' to bookmark research folders
2. Use 'S' to search for keywords in PDF contents
3. Press 'l' to open PDFs in Zathura
4. Use 'T' to toggle preview when reading filenames
5. Press '=' to compare document versions
```

### Workflow 4: Archive Management

```
1. Hover over .zip file to preview contents
2. Press 'l' → "Extract with ouch"
3. Files extract to current directory
4. Use '/' to filter extracted files
5. Select files with 'v' and organize
```

---

## Troubleshooting

### Preview Not Working?

- Check if required packages are installed (they should be via extraPackages)
- Try pressing `T` to toggle preview off and on
- Ensure the file type is supported

### Git Status Not Showing?

- Make sure you're in a git repository
- Run `:git status` to check git is working
- Press `gs` to manually refresh

### Keybinding Not Working?

- Press `?` to see all available keybindings
- Check if another plugin is overriding the key
- Some keys require specific contexts (e.g., `p` needs copied files)

### Plugins Not Loading?

- Rebuild home-manager: `home-manager switch`
- Check Yazi logs: `yazi --debug`
- Ensure plugins are installed in extraPackages

---

## Summary

This Yazi configuration transforms your terminal file management with:

✅ **Smart navigation** with context-aware commands\
✅ **Powerful preview** for code, documents, and archives\
✅ **Git integration** for version control workflows\
✅ **Bookmark system** for instant directory access\
✅ **Tab support** for multitasking\
✅ **Vim-like efficiency** with familiar keybindings

**Remember**: The most important keys to master are:

- `hjkl` for navigation
- `yxp` for copy/cut/paste
- `t` and `[]` for tabs
- `m` and `'` for bookmarks
- `gg` for git

Start with these basics and gradually explore the advanced features. Happy file
managing! 🚀
