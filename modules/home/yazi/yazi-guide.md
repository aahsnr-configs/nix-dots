# Yazi Configuration User Guide

## Table of Contents

1. [What is Yazi?](#what-is-yazi)
2. [Installation](#installation)
3. [Basic Navigation](#basic-navigation)
4. [File Operations](#file-operations)
5. [Advanced Features](#advanced-features)
6. [Plugin Guide](#plugin-guide)
7. [Keybindings Reference](#keybindings-reference)
8. [Bookmark System (YAMB)](#bookmark-system-yamb)
9. [Tips & Tricks](#tips--tricks)

---

## What is Yazi?

Yazi is a blazing-fast terminal file manager written in Rust. It features:

- **Async I/O** for smooth performance
- **Built-in preview** for various file types
- **Vim-like keybindings** for efficient navigation
- **Plugin system** for extensibility
- **Tab support** for multitasking
- **Persistent bookmarks** for instant directory access

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

| Key | Action         | Description                                |
| --- | -------------- | ------------------------------------------ |
| `/` | Smart filter   | Filter files interactively (starts typing) |
| `s` | Search with fd | Find files by name using fd                |
| `S` | Search with rg | Search file contents using ripgrep         |
| `f` | Jump to char   | Vim-like character jump in filename list   |

**Smart Filter**: As you type, the list updates in real-time. Press `Enter` to
select the first match, `Esc` to cancel.

**Jump to Char**: Type `f` followed by a character to quickly jump to the next
file starting with that character.

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

### 13. **YAMB (Bookmarks)** 🔖

- **What it does**: Persistent bookmark manager with fuzzy search
- **Keybindings**:
  - `ma` → Save current directory as bookmark
  - `mg` → Jump to bookmark by key
  - `mG` → Jump to bookmark with fuzzy search (fzf)
  - `md` → Delete bookmark by key
  - `mD` → Delete bookmark with fuzzy search
  - `mC` → Delete all bookmarks
  - `mr` → Rename bookmark by key
  - `mR` → Rename bookmark with fuzzy search
- **Why it's useful**: Instant access to frequently used directories with
  persistence across sessions
- **See**: [Full YAMB guide](#bookmark-system-yamb) below

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

#### Bookmarks (YAMB)

```
ma        → Save bookmark
mg        → Jump to bookmark by key
mG        → Jump to bookmark by fzf
md        → Delete bookmark by key
mD        → Delete bookmark by fzf
mC        → Delete all bookmarks
mr        → Rename bookmark by key
mR        → Rename bookmark by fzf
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

## Bookmark System (YAMB)

### What is YAMB?

YAMB (Yet Another Marks/Bookmarks) is a powerful bookmark management plugin that
allows you to save and quickly jump to frequently used directories. Unlike
temporary marks, YAMB bookmarks **persist across Yazi sessions** and are stored
in `~/.config/yazi/bookmark`.

### Pre-configured Bookmarks

Your configuration comes with 7 default bookmarks ready to use:

| Key | Directory     | Description             |
| --- | ------------- | ----------------------- |
| `h` | `$HOME`       | Home directory          |
| `d` | `~/Downloads` | Downloads folder        |
| `D` | `~/Documents` | Documents folder        |
| `t` | `~/Desktop`   | Desktop                 |
| `c` | `~/.config`   | Configuration directory |
| `p` | `~/Projects`  | Projects folder         |
| `r` | `/`           | Root directory          |

### Basic Usage

#### Saving Bookmarks

**Save current directory:**

1. Navigate to the directory you want to bookmark
2. Press `m` then `a` (think: **m**ark **a**dd)
3. You'll be prompted to assign a key
4. Choose an unused letter/number (e.g., `w` for work directory)
5. Optionally give it a descriptive name

**Example:**

```
Navigate to: ~/work/important-project
Press: ma
Choose key: w
Name: Work Project
✓ Bookmark saved!
```

#### Jumping to Bookmarks

**Method 1: Jump by Key (Fast)**

1. Press `m` then `g` (think: **m**ark **g**o)
2. Press the bookmark key (e.g., `h` for Home)
3. Instantly jump to that directory!

**Example:**

```
Press: mg
Press: h
✓ Jumped to /home/username
```

**Method 2: Jump by Fuzzy Search (Browse)**

1. Press `m` then `G` (Shift+g)
2. Fuzzy search interface appears (powered by fzf)
3. Type to filter bookmarks
4. Use arrow keys to select
5. Press Enter to jump

**Example:**

```
Press: mG
Type: "proj"
Shows: Projects, Work Project, Old Projects
Arrow down to "Projects"
Press Enter
✓ Jumped to ~/Projects
```

### Advanced Operations

#### Deleting Bookmarks

**Delete by Key:**

```
Press: md
Press: w  (the bookmark key to delete)
✓ Bookmark 'w' deleted
```

**Delete by Fuzzy Search:**

```
Press: mD
Search and select bookmark
Press Enter
✓ Bookmark deleted
```

**Delete All Bookmarks:**

```
Press: mC
Confirm: yes
✓ All bookmarks cleared
```

#### Renaming Bookmarks

**Rename by Key:**

```
Press: mr
Press: w  (bookmark to rename)
Enter new name: "Work Main"
✓ Bookmark renamed
```

**Rename by Fuzzy Search:**

```
Press: mR
Search and select bookmark
Enter new name
✓ Bookmark renamed
```

### YAMB Keybinding Summary

All YAMB operations use the `m` prefix (mnemonic: **m**arks):

| Keybinding | Action          | Speed     | Use When                    |
| ---------- | --------------- | --------- | --------------------------- |
| `ma`       | Save bookmark   | -         | Creating new bookmarks      |
| `mg`       | Jump (by key)   | ⚡ Fast   | You know the bookmark key   |
| `mG`       | Jump (by fzf)   | 🔍 Browse | You want to search/browse   |
| `md`       | Delete (by key) | ⚡ Fast   | You know the bookmark key   |
| `mD`       | Delete (by fzf) | 🔍 Browse | You want to search/browse   |
| `mC`       | Delete all      | 💥        | Nuclear option - be careful |
| `mr`       | Rename (by key) | ⚡ Fast   | You know the bookmark key   |
| `mR`       | Rename (by fzf) | 🔍 Browse | You want to search/browse   |

### Customizing Default Bookmarks

To add or modify default bookmarks, edit the `initLua` section in your
`default.nix`:

```lua
-- Add a new default bookmark
table.insert(bookmarks, {
  tag = "Videos",              -- Display name
  path = home_path .. path_sep .. "Videos" .. path_sep,  -- Full path
  key = "v"                    -- Shortcut key (choose unused)
})
```

**Example: Adding multiple custom bookmarks**

```lua
-- Work-related bookmarks
table.insert(bookmarks, { tag = "Work", path = home_path .. "/work/", key = "w" })
table.insert(bookmarks, { tag = "Clients", path = home_path .. "/work/clients/", key = "C" })

-- Media bookmarks
table.insert(bookmarks, { tag = "Videos", path = home_path .. "/Videos/", key = "v" })
table.insert(bookmarks, { tag = "Music", path = home_path .. "/Music/", key = "M" })

-- Development bookmarks
table.insert(bookmarks, { tag = "GitHub", path = home_path .. "/github/", key = "g" })
table.insert(bookmarks, { tag = "Local Sites", path = "/var/www/", key = "s" })
```

### Best Practices

#### 1. **Organize Your Keys**

- **Lowercase** for personal directories (`h`=home, `d`=downloads)
- **Uppercase** for important/work directories (`P`=Projects, `W`=Work)
- **Numbers** for temporary or experimental bookmarks

#### 2. **Use Descriptive Names**

```
❌ Bad:  key="p", name="p"
✅ Good: key="p", name="Personal Projects"

❌ Bad:  key="w", name="work"
✅ Good: key="w", name="Work - Client Projects"
```

#### 3. **Regular Cleanup**

Review your bookmarks monthly:

```
Press: mG
Browse all bookmarks
Delete unused ones with: mD
```

#### 4. **Categorize with Naming**

```
Work - Client A
Work - Client B
Work - Internal
Personal - Photography
Personal - Writing
Dev - Frontend
Dev - Backend
```

This makes fuzzy search more effective!

### YAMB Workflows

#### Workflow 1: Daily Development

```
Morning:
  mg → w    (Jump to work directory)
  gg        (Open Lazygit to check updates)

During work:
  mg → h    (Quick home check)
  mg → d    (Check downloads)
  mg → w    (Back to work)

New project:
  Navigate to ~/work/new-client/
  ma        (Save as bookmark)
  Key: n
  Name: "Work - New Client"
```

#### Workflow 2: Content Organization

```
Download photos:
  mg → d    (Jump to Downloads)
  v         (Visual select photos)
  y         (Copy)

Organize:
  mG → type "photos" → Enter
  p         (Paste)

Create album bookmark:
  ma
  Key: a
  Name: "Photos - Vacation 2024"
```

#### Workflow 3: System Administration

```
# Quick config access
mg → c    (Jump to ~/.config)
l         (Open specific config)

# System logs
Navigate to /var/log
ma
Key: l
Name: "System Logs"

# Now you can always: mg → l (instant logs access)
```

### Troubleshooting YAMB

#### Bookmarks Not Persisting?

**Check storage location:**

```bash
ls -la ~/.config/yazi/bookmark
```

**Verify permissions:**

```bash
chmod 644 ~/.config/yazi/bookmark
```

#### Fuzzy Search Not Working?

**Ensure fzf is installed:**

```bash
which fzf
```

Should show: `/nix/store/.../bin/fzf`

If not, rebuild home-manager:

```bash
home-manager switch
```

#### Key Already Assigned?

When saving a bookmark, if the key is taken:

1. Press a different key
2. Or first delete the old bookmark: `md` → (old key)
3. Then save new bookmark: `ma` → (same key)

#### Bookmark Jump Fails?

If a directory was moved or deleted:

```
Press: mD
Search for the broken bookmark
Delete it
Navigate to new location
Save new bookmark: ma
```

### YAMB vs Other Bookmark Systems

| Feature              | YAMB | Vim Marks | Shell Bookmarks |
| -------------------- | ---- | --------- | --------------- |
| Persists             | ✅   | ❌        | ✅              |
| Fuzzy Search         | ✅   | ❌        | ❌              |
| Key-based Jump       | ✅   | ✅        | ❌              |
| Descriptive Names    | ✅   | ❌        | ✅              |
| Works Across Shells  | ✅   | ❌        | ❌              |
| Integrated with Yazi | ✅   | ❌        | ❌              |

### Pro Tips

#### Tip 1: Bookmark Project Roots

Instead of bookmarking deep paths, bookmark the root:

```
✅ Bookmark: ~/Projects/
Then navigate: l → client-a → src
```

This keeps your bookmark list clean!

#### Tip 2: Use Both Methods

- `mg` (by key) for your top 10 most-used directories
- `mG` (by fzf) for everything else

#### Tip 3: Backup Your Bookmarks

```bash
# Backup
cp ~/.config/yazi/bookmark ~/backup-bookmarks

# Restore
cp ~/backup-bookmarks ~/.config/yazi/bookmark
```

#### Tip 4: Share Bookmarks Across Machines

Add to your dotfiles repo:

```bash
cd ~/dotfiles
cp ~/.config/yazi/bookmark .
git add bookmark
git commit -m "Add Yazi bookmarks"
git push
```

On another machine:

```bash
cp ~/dotfiles/bookmark ~/.config/yazi/bookmark
```

---

## Tips & Tricks

### 1. **Quick Directory Navigation with Bookmarks**

```bash
# First time setup - bookmark your most used directories
cd ~/Projects && yazi  # Press 'ma', key: 'p'
cd ~/Documents && yazi # Press 'ma', key: 'D'
cd ~/Downloads && yazi # Press 'ma', key: 'd'

# Now from anywhere:
Press: mg → p  (instant ~/Projects access)
Press: mg → D  (instant ~/Documents access)
Press: mg → d  (instant ~/Downloads access)

# Or use fuzzy search:
Press: mG → type "proj" → Enter
```

### 2. **Bulk File Operations**

```
1. Press 'v' to enter visual mode
2. Use 'j/k' to select multiple files
3. Press 'y' to copy or 'x' to cut
4. Navigate to destination (use 'mg' for bookmarked locations!)
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
1. Navigate to git repository (or mg to bookmarked repo)
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

### 8. **Tab Workflow with Bookmarks**

```
# Organize projects across tabs:
Tab 1: mg → p (~/Projects/)
Tab 2: mg → D (~/Documents/)
Tab 3: mg → d (~/Downloads/)
Tab 4: mg → c (~/.config/)

# Switch instantly with [ ] or 1-4 keys
# Each tab remembers its location!
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

### 11. **Bookmark Workflows**

```
# Morning routine:
mg → w    (Work directory)
gg        (Check git status)

# Quick config edits:
mg → c    (Config directory)
/nvim     (Filter for nvim config)
l         (Open in Neovim)

# Download organization:
mg → d    (Downloads)
v         (Select files)
y         (Copy)
mG        (Fuzzy search destinations)
p         (Paste)
```

---

## Common Workflows

### Workflow 1: Code Project Management with Bookmarks

```
1. mg → p (Jump to bookmarked ~/Projects)
2. l to enter project directory
3. Press 'ma' to bookmark this specific project
4. Press 'gg' to check git status with Lazygit
5. Use 'cm' to fix permissions on scripts
6. Press 'l' to open files in Neovim
7. Use tabs to manage multiple projects
8. Use 'mg' to jump between bookmarked projects
```

### Workflow 2: Media Organization with Smart Bookmarks

```
1. mg → d (Jump to Downloads)
2. Press 'v' to select photos/videos
3. Press 'y' to copy
4. Press 'mG' and search for album bookmark
5. Press 'p' to paste directly
6. Create new album: a → album-name
7. Bookmark it: ma → key: 'a' → "Album 2024"
```

### Workflow 3: Document Research

```
1. mg → D (Jump to Documents)
2. Use 'S' to search for keywords in PDF contents
3. Press 'l' to open PDFs in Zathura
4. Use 'T' to toggle preview when reading filenames
5. Press '=' to compare document versions
6. Bookmark research folders for quick access
```

### Workflow 4: Archive Management

```
1. mg → d (Downloads with archives)
2. Hover over .zip file to preview contents
3. Press 'l' → "Extract with ouch"
4. Files extract to current directory
5. Use '/' to filter extracted files
6. Select files with 'v' and organize
7. mg → destination bookmark
8. p to paste
```

### Workflow 5: Multi-Project Development

```
Setup (one time):
  Navigate to ~/work/project-a
  ma → key: 1 → "Client A"
  
  Navigate to ~/work/project-b
  ma → key: 2 → "Client B"
  
  Navigate to ~/work/project-c
  ma → key: 3 → "Client C"

Daily use:
  mg → 1  (Client A)
  gg      (Check git)
  mg → 2  (Client B)
  gg      (Check git)
  mg → 3  (Client C)
  gg      (Check git)

Switch rapidly between projects with muscle memory!
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

### YAMB Bookmarks Not Saving?

- Check file permissions: `ls -la ~/.config/yazi/bookmark`
- Ensure directory exists: `mkdir -p ~/.config/yazi`
- Check Yazi logs: `yazi --debug`
- Verify fzf is installed: `which fzf`

### Bookmark Jump Shows "Directory Not Found"?

- The bookmarked directory may have been moved or deleted
- Press `mD` to delete the broken bookmark
- Navigate to the new location and create a fresh bookmark

---

## Summary

This Yazi configuration transforms your terminal file management with:

✅ **Smart navigation** with context-aware commands\
✅ **Powerful preview** for code, documents, and archives\
✅ **Git integration** for version control workflows\
✅ **Persistent bookmark system (YAMB)** for instant directory access\
✅ **Tab support** for multitasking\
✅ **Vim-like efficiency** with familiar keybindings

**Remember**: The most important keys to master are:

- `hjkl` for navigation
- `yxp` for copy/cut/paste
- `ma` and `mg`/`mG` for bookmarks ⭐
- `t` and `[]` for tabs
- `gg` for git

### Quick Start Checklist

1. ✅ Launch Yazi: `yazi`
2. ✅ Bookmark your home: `mg → h`
3. ✅ Navigate to Projects: `cd ~/Projects`
4. ✅ Save a bookmark: `ma → p → "Projects"`
5. ✅ Test jump: `mg → p`
6. ✅ Explore fuzzy search: `mG`
7. ✅ Master the basics: `hjkl`, `yxp`
8. ✅ Try git integration: `gg`

Start with these basics and gradually explore the advanced features. The
bookmark system alone will save you hours of navigation time! Happy file
managing! 🚀
