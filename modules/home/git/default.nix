# ~/nix-dots/modules/home/git/default.nix
{ pkgs, lib, ... }:

let
  # Define all GitHub account configurations in one place.
  accounts = {
    configs = {
      userName = "aahsnr-configs";
      userEmail = "ahsanur041@proton.me";
      keyName = "aahsnr_configs"; # Used for the id_ed25519_* file name
    };
    personal = {
      userName = "aahsnr-personal";
      userEmail = "ahsanur041@gmail.com";
      keyName = "aahsnr_personal";
    };
    work = {
      userName = "aahsnr-work";
      userEmail = "aahsnr041@proton.me";
      keyName = "aahsnr_work";
    };
    common = {
      userName = "aahsnr-common";
      userEmail = "ahsan.05rahman@gmail.com";
      keyName = "aahsnr_common";
    };
  };

  # Specify which account from the 'accounts' set should be the default.
  defaultAccount = "configs";

in
{
  # Configure SSH to use different keys for different GitHub accounts.
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    # Create a Match block for each account to associate a unique host alias
    # with a specific SSH key.
    matchBlocks = lib.mapAttrs' (name: cfg: {
      # Creates host aliases like "github.com-aahsnr-configs"
      name = "github.com-${cfg.userName}";
      value = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519_${cfg.keyName}";
        extraOptions = {
          AddKeysToAgent = "yes";
          IdentitiesOnly = "yes";
        };
      };
    }) accounts;
  };

  # Configure Git with multi-account support.
  programs.git = {
    enable = true;
    package = pkgs.gitFull;

    # Set default user details from the designated default account.
    userName = accounts.${defaultAccount}.userName;
    userEmail = accounts.${defaultAccount}.userEmail;

    extraConfig = {
      init.defaultBranch = "main";
      # By default, rewrite github.com URLs to use the default account's SSH alias.
      "url \"git@github.com-${accounts.${defaultAccount}.userName}:\"" = {
        insteadOf = "git@github.com:";
      };
    };

    # Generate conditional includes for all non-default accounts. These settings
    # will apply only when inside a repository located under a specific directory.
    includes = lib.mapAttrsToList (name: cfg: {
      condition = "gitdir:~/git-repos/${name}/";
      contents = {
        user = {
          name = cfg.userName;
          email = cfg.userEmail;
        };
        # When in a specific directory (e.g., ~/git-repos/personal/),
        # rewrite github.com URLs to use that directory's corresponding SSH alias.
        url = {
          "git@github.com-${cfg.userName}:" = {
            insteadOf = "git@github.com:";
          };
        };
      };
    }) (lib.filterAttrs (name: _: name != defaultAccount) accounts);
  };

  # Configure the GitHub CLI (gh).
  programs.gh = {
    enable = true;
    settings = {
      git_protocol = "ssh";
      editor = "nvim";
      # Generate 'gh auth switch' aliases for each account for easy switching.
      aliases = lib.mapAttrs' (name: cfg: {
        name = "auth-${name}";
        value = "auth switch --hostname github.com --user ${cfg.userName}";
      }) accounts;
    };
  };

  # Create a helper script to automate the setup of SSH keys.
  home.file.".local/bin/setup-github-keys" =
    let
      # Pre-generate the dynamic parts of the script for clarity.
      generateKeyCommands = lib.mapAttrsToList (
        name: cfg: ''generate_key "${cfg.keyName}" "${cfg.userEmail}"''
      ) accounts;

      publicKeyInstructions = lib.mapAttrsToList (
        name: cfg: ''echo "   - ${lib.strings.toSentenceCase name}:  cat ~/.ssh/id_ed25519_${cfg.keyName}.pub"''
      ) accounts;

      testSshCommands = lib.mapAttrsToList (
        name: cfg: ''echo "   ssh -T git@github.com-${cfg.userName}"''
      ) accounts;

      projectDirectories = lib.concatMapStringsSep " " (name: "~/git-repos/${name}") (
        lib.attrNames accounts
      );

    in
    {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        set -euo pipefail

        echo "Setting up SSH keys for multiple GitHub accounts..."
        mkdir -p ~/.ssh
        chmod 700 ~/.ssh

        generate_key() {
            local key_name="$1"
            local email="$2"
            local key_path="$HOME/.ssh/id_ed25519_$key_name"

            if [[ ! -f "$key_path" ]]; then
                echo "Generating Ed25519 SSH key for '$key_name'..."
                ssh-keygen -t ed25519 -C "$email" -f "$key_path" -N ""
                chmod 600 "$key_path"
                chmod 644 "$key_path.pub"
                echo "-> Generated key: $key_path"
            else
                echo "-> Key for '$key_name' already exists: $key_path"
            fi
        }

        # --- IMPORTANT ---
        # These SSH key generation calls are created from your Nix config.
        ${lib.concatStringsSep "\n" generateKeyCommands}

        echo ""
        echo "✅ Setup complete!"
        echo ""
        echo "Next Steps:"
        echo "1. Add the public keys to your respective GitHub accounts:"
        ${lib.concatStringsSep "\n" publicKeyInstructions}
        echo ""
        echo "2. Authenticate each account with the GitHub CLI (run 'gh auth login' multiple times)."
        echo "   gh auth login --hostname github.com"
        echo ""
        echo "3. Test your SSH connections:"
        ${lib.concatStringsSep "\n" testSshCommands}
        echo ""
        echo "4. Create your project directories:"
        echo "   mkdir -p ${projectDirectories}"
      '';
    };
}
