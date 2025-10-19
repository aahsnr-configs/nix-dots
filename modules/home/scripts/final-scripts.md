Of course. By separating the scripts into their own files, the main Nix configuration becomes much cleaner and easier to manage. This is an excellent practice for maintainability.

Here is the complete guide, including the recommended file locations, the content for each individual script file, the updated Home Manager module that reads them, and the answer to your question about file permissions.

---

### Recommended File Structure

It's best practice to keep your scripts organized. A great way to do this is to create a dedicated `scripts` directory alongside your Nix configuration file. Assuming your Home Manager configuration is located at `~/nix-config/home-manager/`, you should place the scripts in a new subdirectory like this:

```
~/nix-config/
└── home-manager/
    ├── default.nix
    ├── scripts.nix   <-- Your main module file
    └── scripts/      <-- Directory for all script files
        ├── safe-rm.fish
        ├── nuke-nvim.fish
        ├── setup-github-keys.fish
        ├── launch_first_available.fish
        ├── fuzzel-emoji.fish
        ├── wsaction.fish
        ├── fe.fish
        ├── se.fish
        ├── fkill.fish
        ├── fconf.fish
        ├── fp.fish
        └── fssh.fish
```

### Script Files

Here is the content for each individual script file. You should save each one at the path specified in its title.

---

#### `~/nix-config/home-manager/scripts/safe-rm.fish`

```fish
#!/usr/bin/env fish
set -l TRASH_DIR "$HOME/.local/share/trash/files"

function main
    if test (count $argv) -eq 0
        echo "Error: No files or directories specified." >&2
        echo "Usage: (status filename) <file1> [<file2> ...]" >&2
        exit 1
    end

    set -l targets
    for item in $argv
        if not test -e "$item"
            echo "Warning: '$item' not found, skipping." >&2
            continue
        end

        mkdir -p "$TRASH_DIR"
        if test (realpath -- "$item") = (realpath -- "$TRASH_DIR")
            echo "Warning: Cannot move the trash directory into itself. Skipping '$item'." >&2
            continue
        end
        set -a targets "$item"
    end

    if test (count $targets) -eq 0
        echo "No valid files or directories to move to trash."
        exit 0
    end

    echo "The following items will be moved to the trash:"
    printf "  - %s\n" $targets
    echo "Trash location: $TRASH_DIR"
    echo ""

    read -p "Are you sure you want to continue? (Y/n): " -l REPLY
    echo ""

    if test -n "$REPLY"
        echo "Operation cancelled by user."
        exit 1
    end

    echo "Moving items to trash..."
    for item in $targets
        set -l trashed_name (basename "$item")-(date +%s)-(head -c 5 /dev/urandom | tr -dc A-Za-z-0-9)
        echo " - '$item' -> '$trashed_name'"
        mv -- "$item" "$TRASH_DIR/$trashed_name"
    end

    echo ""
    echo "✅ Operation complete."
    echo "To restore, check the trash directory: $TRASH_DIR"
end

main $argv
```

---

#### `~/nix-config/home-manager/scripts/nuke-nvim.fish`

```fish
#!/usr/bin/env fish
set -l NVIM_DIRS \
    "$HOME/.config/nvim" \
    "$HOME/.local/state/nvim" \
    "$HOME/.local/share/nvim"

function main
    echo "This script will permanently delete the following Neovim directories:"
    for dir in $NVIM_DIRS
        echo "  - $dir"
    end
    echo ""

    read -p "Are you sure you want to continue? (Y/n): " -l REPLY
    echo ""

    if test -n "$REPLY"
        echo "Operation cancelled by user."
        exit 1
    end

    echo ""
    echo "Starting removal process..."

    for dir in $NVIM_DIRS
        if test -d "$dir"
            echo "Removing $dir..."
            rm -rf "$dir"
        else
            echo "Directory $dir not found, skipping."
        end
    end

    echo ""
    echo "Successfully removed Neovim directories."
end

main
```

---

#### `~/nix-config/home-manager/scripts/setup-github-keys.fish`

```fish
#!/usr/bin/env fish
echo "Setting up SSH keys for multiple GitHub accounts..."
mkdir -p ~/.ssh
chmod 700 ~/.ssh

function generate_key --argument key_name email
    set -l key_path "$HOME/.ssh/id_rsa_$key_name"

    if not test -f "$key_path"
        echo "Generating RSA 4096 SSH key for '$key_name'..."
        ssh-keygen -t rsa -b 4096 -C "$email" -f "$key_path" -N ""
        chmod 600 "$key_path"
        chmod 644 "$key_path.pub"
        echo "-> Generated key: $key_path"
    else
        echo "-> Key for '$key_name' already exists: $key_path"
    end
end

generate_key "aahsnr_configs" "ahsanur041@proton.me"
generate_key "aahsnr_personal" "ahsanur041@gmail.com"
generate_key "aahsnr_work" "aahsnr041@proton.me"
generate_key "aahsnr_common" "ahsan.05rahman@gmail.com"

echo ""
echo "✅ Setup complete!"
echo ""
echo "Next Steps:"
echo "1. Add the public keys to your respective GitHub accounts:"
echo "   - Configs:  cat ~/.ssh/id_rsa_aahsnr_configs.pub"
echo "   - Personal: cat ~/.ssh/id_rsa_aahsnr_personal.pub"
echo "   - Work:     cat ~/.ssh/id_rsa_aahsnr_work.pub"
echo "   - Common:   cat ~/.ssh/id_rsa_aahsnr_common.pub"
echo ""
echo "2. Authenticate each account with the GitHub CLI (run these one by one):"
echo "   gh auth login --hostname github.com"
echo "   (You will need to do this for each of your accounts)"
echo ""
echo "3. Test your SSH connections:"
echo "   ssh -T git@github.com-aahsnr-configs"
echo "   ssh -T git@github.com-aahsnr-personal"
echo "   ssh -T git@github.com-aahsnr-work"
echo "   ssh -T git@github.com-aahsnr-common"
echo ""
echo "4. Create your project directories:"
echo "   mkdir -p ~/git-repos/configs ~/git-repos/personal ~/git-repos/work ~/git-repos/common"
```

---

#### `~/nix-config/home-manager/scripts/launch_first_available.fish`

```fish
#!/usr/bin/env fish
if test (count $argv) -eq 0
    echo "Usage: (status filename) \"command1\" [\"command2 with-args\" ...]" >&2
    exit 1
end

for cmd_with_args in $argv
    if test -z "$cmd_with_args"
        continue
    end

    set -l command_name (string split -n -m 1 ' ' -- "$cmd_with_args")[1]

    if command -v "$command_name" &>/dev/null
        echo "Found command: '$command_name'. Launching..."
        eval "$cmd_with_args" &
        exit 0
    end
end

echo "Error: None of the specified commands were found." >&2
exit 1
```

---

#### `~/nix-config/home-manager/scripts/fuzzel-emoji.fish`

```fish
#!/usr/bin/env fish
set -l MODE $argv[1]
if test -z "$MODE"
    set MODE "type"
end

set -l DATA "
😀 grinning face face smile happy joy :D grin
😃 grinning face with big eyes face happy joy haha :D :) smile funny
😄 grinning face with smiling eyes face happy joy funny haha laugh like :D :) smile
😁 beaming face with smiling eyes face happy smile joy kawaii
😆 grinning squinting face happy joy lol satisfied haha face glad XD laugh
😅 grinning face with sweat face hot happy laugh sweat smile relief
🤣 rolling on the floor laughing face rolling floor laughing lol haha rofl
😂 face with tears of joy face cry tears weep happy happytears haha
🙂 slightly smiling face face smile
🙃 upside down face face flipped silly smile
😉 winking face face happy mischievous secret ;) smile eye
😊 smiling face with smiling eyes face smile happy flushed crush embarrassed shy joy
😇 smiling face with halo face angel heaven halo
🥰 smiling face with hearts face love like affection valentines infatuation crush hearts adore
😍 smiling face with heart eyes face love like affection valentines infatuation crush heart
🤩 star struck face smile starry eyes grinning
😘 face blowing a kiss face love like affection valentines infatuation kiss
😗 kissing face love like face 3 valentines infatuation kiss
☺️ smiling face face blush massage happiness
😚 kissing face with closed eyes face love like affection valentines infatuation kiss
😙 kissing face with smiling eyes face affection valentines infatuation kiss
😋 face savoring food happy joy tongue smile face silly yummy nom delicious savouring
😛 face with tongue face prank childish playful mischievous smile tongue
😜 winking face with tongue face prank childish playful mischievous smile wink tongue
🤪 zany face face goofy crazy
😝 squinting face with tongue face prank playful mischievous smile tongue
🤑 money mouth face face rich dollar money
🤗 hugging face face smile hug
🤭 face with hand over mouth face whoops shock surprise
🤫 shushing face face quiet shhh
🤔 thinking face face hmmm think consider
🤐 zipper mouth face face sealed zipper secret
🤨 face with raised eyebrow face distrust scepticism disapproval disbelief surprise
😐 neutral face indifference meh :| neutral
😑 expressionless face face indifferent - - meh deadpan
😶 face without mouth face hellokitty
😏 smrking face face smile mean prank smug sarcasm
😒 unamused face indifference bored straight face serious sarcasm unimpressed skeptical dubious side eye
🙄 face with rolling eyes face eyeroll frustrated
😬 grimacing face face grimace teeth
🤥 lying face face lie pinocchio
😌 relieved face face relaxed phew massage happiness
😔 pensive face face sad depressed upset
😪 sleepy face face tired rest nap
🤤 drooling face face
😴 sleeping face face tired sleepy night zzz
😷 face with medical mask face sick ill disease
🤒 face with thermometer sick temperature thermometer cold fever
🤕 face with head bandage injured clumsy bandage hurt
🤢 nauseated face face vomit gross green sick throw up ill
🤮 face vomiting face sick
🤧 sneezing face face gesundheit sneeze sick allergy
🥵 hot face face feverish heat red sweating
🥶 cold face face blue freezing frozen frostbite icicles
🥴 woozy face face dizzy intoxicated tipsy wavy
😵 dizzy face spent unconscious xox dizzy
🤯 exploding head face shocked mind blown
🤠 cowboy hat face face cowgirl hat
🥳 partying face face celebration woohoo
😎 smiling face with sunglasses face cool smile summer beach sunglass
🤓 nerd face face nerdy geek dork
🧐 face with monocle face stuffy wealthy
😕 confused face face indifference huh weird hmmm :/
😟 worried face face concern nervous :(
🙁 slightly frowning face face frowning disappointed sad upset
☹️ frowning face face sad upset frown
😮 face with open mouth face surprise impressed wow whoa :O
😯 hushed face face woo shh
😲 astonished face face xox surprised poisoned
😳 flushed face face blush shy flattered sex
🥺 pleading face face begging mercy
😦 frowning face with open mouth face aw what
😧 anguished face face stunned nervous
😨 fearful face face scared terrified nervous oops huh
😰 anxious face with sweat face nervous sweat
😥 sad but relieved face face phew sweat nervous
😢 crying face face tears sad depressed upset :'(
😭 loudly crying face face cry tears sad upset depressed sob
😱 face screaming in fear face munch scared omg
😖 confounded face face confused sick unwell oops :S
😣 persevering face face sick no upset oops
😞 disappointed face face sad upset depressed :(
😓 downcast face with sweat face hot sad tired exercise
😩 weary face face tired sleepy sad frustrated upset
😫 tired face sick whine upset frustrated
🥱 yawning face tired sleepy
😤 face with steam from nose face gas phew proud pride
😡 pouting face angry mad hate despise
😠 angry face mad face annoyed frustrated
🤬 face with symbols on mouth face swearing cursing cssing profanity expletive
😈 smiling face with horns devil horns
👿 angry face with horns devil angry horns
💀 skull dead skeleton creepy death
☠️ skull and crossbones poison danger deadly scary death pirate evil
💩 pile of poo hankey shitface fail turd shit
🤡 clown face face
👹 ogre monster red mask halloween scary creepy devil demon japanese ogre
👺 goblin red evil mask monster scary creepy japanese goblin
👻 ghost halloween spooky scary
👽 alien UFO paul weird outer space
👾 alien monster game arcade play
🤖 robot computer machine bot
😺 grinning cat animal cats happy smile
😸 grinning cat with smiling eyes animal cats smile
😹 cat with tears of joy animal cats haha happy tears
😻 smiling cat with heart eyes animal love like affection cats valentines heart
😼 cat with wry smile animal cats smirk
😽 kissing cat animal cats kiss
🙀 weary cat animal cats munch scared scream
😿 crying cat animal tears weep sad cats upset cry
😾 pouting cat animal cats
🙈 see no evil monkey monkey animal nature haha
🙉 hear no evil monkey animal monkey nature
🙊 speak no evil monkey monkey animal nature omg
💋 kiss mark face lips love like affection valentines
💌 love letter email like affection envelope valentines
💘 heart with arrow love like heart affection valentines
💝 heart with ribbon love valentines
💖 sparkling heart love like affection valentines
💗 growing heart like love affection valentines pink
💓 beating heart love like affection valentines pink heart
💞 revolving hearts love like affection valentines
💕 two hearts love like affection valentines heart
💟 heart decoration purple-square love like
❣️ heart exclamation decoration love
💔 broken heart sad sorry break heart heartbreak
❤️ red heart love like valentines
🧡 orange heart love like affection valentines
💛 yellow heart like like affection valentines
💚 green heart love like affection valentines
💙 blue heart love like affection valentines
💜 purple heart love like affection valentines
🤎 brown heart coffee
🖤 black heart evil
🤍 white heart pure
"

set -l selected_line (echo -e "$DATA" | string trim --left | fuzzel --match-mode fzf --dmenu)

if test -z "$selected_line"
    exit 0
end

set -l emoji (string split -m 1 ' ' -- "$selected_line")[1]

switch "$MODE"
    case type
        wtype "$emoji"; or wl-copy "$emoji"
    case copy
        wl-copy "$emoji"
    case both
        wtype "$emoji" &>/dev/null
        wl-copy "$emoji"
    case '*'
        echo "Usage: (status filename) [type|copy|both]" >&2
        exit 1
end
```

---

#### `~/nix-config/home-manager/scripts/wsaction.fish`

```fish
#!/usr/bin/env fish
if test "$argv[1]" = '-g'
 set group
 set -e $argv[1]
end

if test (count $argv) -ne 2
 echo 'Wrong number of arguments. Usage: ./wsaction.fish [-g] <dispatcher> <workspace>'
 exit 1
end

set -l active_ws (hyprctl activeworkspace -j | jq -r '.id')

if set -q group
 # Move to group
 hyprctl dispatch $argv[1] (math "($argv[2] - 1) * 10 + $active_ws % 10")
else
 # Move to ws in group
 hyprctl dispatch $argv[1] (math "floor(($active_ws - 1) / 10) * 10 + $argv[2]")
end
```

---

#### `~/nix-config/home-manager/scripts/fe.fish`

```fish
#!/usr/bin/env fish
set selected (fd --type f --hidden --follow --exclude .git . | fzf \
    --height "80%" --layout "reverse" --info "inline" --border "rounded" \
    --preview 'bat --style=numbers --color=always --line-range :500 {}' \
    --query="$argv[1]" --select-1 --exit-0 --no-multi)

if test -n "$selected"
    $EDITOR "$selected"
end
```

---

#### `~/nix-config/home-manager/scripts/se.fish`

```fish
#!/usr/bin/env fish
if not set -q argv[1]
    echo "Usage: se <search_pattern>"
    exit 1
end

set selection (rg --line-number --no-heading --smart-case "$argv[1]" | fzf \
    --height "80%" --layout "reverse" --info "inline" --border "rounded" \
    --delimiter ':' --no-multi \
    --preview 'bat --style=numbers --color=always --highlight-line {2} {1}' \
    --preview-window 'up,60%,border-bottom,+{2}+3/3,~3')

if test -n "$selection"
    set file (echo "$selection" | cut -d: -f1)
    set line_num (echo "$selection" | cut -d: -f2)
    $EDITOR "$file" "+$line_num"
end
```

---

#### `~/nix-config/home-manager/scripts/fkill.fish`

```fish
#!/usr/bin/env fish
set pid (ps -ef | sed 1d | grep -v "$0" | fzf --height "40%" --layout "reverse" --no-multi | awk '{print $2}')

if test -n "$pid"
    set process_name (ps -p "$pid" -o comm=)
    read --nchars 1 --prompt-str "Are you sure you want to kill process $pid ($process_name)? [y/N] " reply
    echo
    if string match -qr '^[Yy]$' -- "$reply"
        kill -9 "$pid"
        echo "Process $pid ($process_name) killed."
    else
        echo "Kill operation aborted."
    end
end
```

---

#### `~/nix-config/home-manager/scripts/fconf.fish`

```fish
#!/usr/bin/env fish
set config_dirs "$HOME" "$HOME/.config"

set selected (fd --type f --hidden . $config_dirs | fzf \
    --height "60%" --layout "reverse" --border "rounded" \
    --prompt="Edit Config > " --no-multi \
    --preview 'bat --style=numbers --color=always {}')

if test -n "$selected"
    $EDITOR "$selected"
end
```

---

#### `~/nix-config/home-manager/scripts/fp.fish`

```fish
#!/usr/bin/env fish
fd --hidden --follow --exclude .git | fzf --height "80%" --layout "reverse" --border "rounded" \
    --preview-window "right:50%:wrap" \
    --preview '
        set file_path "{}"
        if test -d "$file_path"
            eza --color=always --tree --level=2 "$file_path" \
            || tree -C -L 2 "$file_path" \
            || ls -lF --color=always "$file_path"
        else if string match --quiet "image/*" -- (file --mime --brief "$file_path")
            chafa --size (tput cols)x(tput lines) "$file_path"
        else
            bat --style=numbers --color=always "$file_path"
        end
    '
```

---

#### `~/nix-config/home-manager/scripts/fssh.fish`

```fish
#!/usr/bin/env fish
if not test -f "$HOME/.ssh/config"
    echo "SSH config file not found at ~/.ssh/config"
    exit 1
end

set host (grep '^Host ' "$HOME/.ssh/config" | awk '{print $2}' | grep -v '*' | fzf \
    --height "20%" --layout "reverse" --border "rounded" \
    --prompt="SSH to > " --no-multi)

if test -n "$host"
    ssh "$host"
end
```

---

### Home Manager Module

This is the updated `scripts.nix` file. It now uses `builtins.readFile` to load each script from the `scripts/` directory. This makes the Nix code clean and delegates the script logic to the individual files where it belongs.

#### `~/nix-config/home-manager/scripts.nix`

```nix
{ pkgs, ... }:

let
  # A helper function to create a Fish script as a Nix package.
  mkFishScript = { name, text, runtimeInputs ? [ ] }:
    pkgs.writeShellApplication {
      inherit name text;
      interpreter = "${pkgs.fish}/bin/fish";
      inherit runtimeInputs;
    };

  # All scripts are defined here, reading their content from the ./scripts directory.
  scripts = {
    safe-rm = mkFishScript {
      name = "safe-rm";
      text = builtins.readFile ./safe-rm.fish;
    };

    nuke-nvim = mkFishScript {
      name = "nuke-nvim";
      text = builtins.readFile ./nuke-nvim.fish;
    };

    setup-github-keys = mkFishScript {
      name = "setup-github-keys";
      text = builtins.readFile ./setup-github-keys.fish;
      runtimeInputs = [ pkgs.openssh pkgs.gh ];
    };

    launch-first-available = mkFishScript {
      name = "launch_first_available";
      text = builtins.readFile ./launch_first_available.fish;
    };

    fuzzel-emoji = mkFishScript {
      name = "fuzzel-emoji";
      text = builtins.readFile ./fuzzel-emoji.fish;
      runtimeInputs = [ pkgs.fuzzel pkgs.wtype pkgs.wl-clipboard ];
    };

    wsaction = mkFishScript {
      name = "wsaction";
      text = builtins.readFile ./wsaction.fish;
      runtimeInputs = [ pkgs.hyprland pkgs.jq ];
    };

    fe = mkFishScript {
      name = "fe";
      text = builtins.readFile ./fe.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.bat ];
    };

    se = mkFishScript {
      name = "se";
      text = builtins.readFile ./se.fish;
      runtimeInputs = [ pkgs.ripgrep pkgs.fzf pkgs.bat ];
    };

    fkill = mkFishScript {
      name = "fkill";
      text = builtins.readFile ./fkill.fish;
      runtimeInputs = [ pkgs.fzf pkgs.procps pkgs.gawk ];
    };

    fconf = mkFishScript {
      name = "fconf";
      text = builtins.readFile ./fconf.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.bat ];
    };

    fp = mkFishScript {
      name = "fp";
      text = builtins.readFile ./fp.fish;
      runtimeInputs = [ pkgs.fd pkgs.fzf pkgs.eza pkgs.tree pkgs.chafa pkgs.bat pkgs.file ];
    };

    fssh = mkFishScript {
      name = "fssh";
      text = builtins.readFile ./fssh.fish;
      runtimeInputs = [ pkgs.fzf pkgs.openssh pkgs.gawk ];
    };
  };

in
{
  # Install all the scripts defined above into the user's environment.
  home.packages = pkgs.lib.attrValues scripts;
}
```

### Fish Function

```fish
function fh
    history merge
    set command (history | fzf --query="$argv[1]" --height="40%" --layout="reverse" --no-multi)

    if test -n "$command"
        commandline --replace "$command"
    end
end
```

---
