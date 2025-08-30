Finding and studying popular NixOS configurations is one of the best ways to learn from the community. These repositories, often called "dotfiles," showcase a wide range of setups, from minimal to highly customized, and frequently leverage features like Nix flakes and Home Manager.

Here's an updated list of some of the most prominent NixOS configurations on GitHub, often judged by their star count and community impact.

-----

### 1 **Misterio77/nix-config** 💻

A highly recommended and comprehensive NixOS configuration known for its clean structure and advanced features. It's a great example for those wanting to dive deep into declarative system management.

  * **Key Features**: Uses Nix flakes, implements opt-in persistence with the `impermanence` module, manages secrets with `sops-nix`, and includes detailed configurations for Wayland window managers like Sway and Hyprland.
  * **Repository Link**: [github.com/Misterio77/nix-config](https://github.com/Misterio77/nix-config)

-----

### 2. **hlissner/dotfiles** ⚙️

This configuration is widely recognized, especially for its connection to the Doom Emacs project. It showcases a modular approach to building a NixOS system.

  * **Key Features**: Known for its clean, modular structure, heavy use of Home Manager for user-specific settings, and a detailed configuration for Doom Emacs.
  * **Repository Link**: [github.com/hlissner/dotfiles](https://github.com/hlissner/dotfiles)

-----

### 3. **dustinlyons/nixos-config** 🍎

A popular configuration focusing on a declarative experience across multiple operating systems, making it a great resource for macOS users who want to manage their system with Nix.

  * **Key Features**: Supports both NixOS and macOS using `nix-darwin`, provides starter templates for beginners, and includes secrets management as a full-featured option.
  * **Repository Link**: [github.com/dustinlyons/nixos-config](https://github.com/dustinlyons/nixos-config)

-----

### 4. **mitchellh/nixos-config** ☁️

Authored by Mitchell Hashimoto, the creator of HashiCorp tools, this repository is one of the most starred NixOS configurations. It's particularly notable for its focus on a virtual machine-based development workflow.

  * **Key Features**: Built around a declarative VM environment for reproducible development, works across various host operating systems (macOS, Windows), and is well-documented.
  * **Repository Link**: [github.com/mitchellh/nixos-config](https://github.com/mitchellh/nixos-config)

-----

### 5. **Mic92/dotfiles** 🐧

A robust and well-maintained configuration from a long-time NixOS contributor. It's a great example of best practices within the community.

  * **Key Features**: A clean, flake-based configuration that demonstrates declarative containers (`nixos-containers`) for managing services, and often serves as a reference for complex setups.
  * **Repository Link**: [github.com/Mic92/dotfiles](https://github.com/Mic92/dotfiles)

-----

### 6. **colemickens/nixcfg** ☕

This is a mature and highly modular configuration that has evolved over time. It's an excellent resource for seeing how a large NixOS setup is organized.

  * **Key Features**: Divided into numerous modules for different applications and services, includes configurations for both server and desktop environments, and provides a detailed blueprint for building a complex system.
  * **Repository Link**: [github.com/colemickens/nixcfg](https://github.com/colemickens/nixcfg)

-----

### 7. **wimpysworld/nix-config** 🍏

This configuration, from the creator of Ubuntu MATE, is highly starred and valuable for its focus on multi-system management, including macOS.

  * **Key Features**: A great example of using `nix-darwin` for macOS, features a well-documented setup for the Pantheon desktop environment, and uses Home Manager extensively.
  * **Repository Link**: [github.com/wimpysworld/nix-config](https://github.com/wimpysworld/nix-config)

-----

### 8. **ryant4yin/nix-config** 📖

While Ryan Yin is more widely known for the "NixOS & Flakes Book," his personal configuration is a great practical example that complements his book.

  * **Key Features**: Provides a practical example of the concepts taught in his book, is structured with Nix flakes, and is a good starting point for those who have read his tutorial.
  * **Repository Link**: [github.com/ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)

-----

### 9. **srid/nixos-config** 🔑

A popular and minimalist configuration that focuses on simplicity while leveraging powerful Nix features.

  * **Key Features**: A KISS (Keep It Simple, Stupid) configuration that supports both NixOS and macOS, uses `flake-parts` for a structured layout, and relies on `agenix` for secrets management.
  * **Repository Link**: [github.com/srid/nixos-config](https://github.com/srid/nixos-config)

-----

### 10. **fufexan/dotfiles** 🖥️

This configuration is a popular example of a user who has meticulously crafted a Hyprland-based desktop environment using Nix. It's a great resource for ricing and learning about declarative desktop setups.

  * **Key Features**: Highly focused on a Hyprland setup, uses Home Manager for user configuration, and is known for its clean, well-structured approach.
  * **Repository Link**: [github.com/fufexan/dotfiles](https://github.com/fufexan/dotfiles)

-----

### 11. **maximbaz/dotfiles** 🎹

This is a comprehensive and well-documented configuration from a prolific open-source contributor. It's a goldmine for those interested in a full desktop setup with many integrations.

  * **Key Features**: Features configurations for sway, Kitty, Zsh, and more. It has a focus on desktop integration and is a great resource for declarative ricing.
  * **Repository Link**: [github.com/maximbaz/dotfiles](https://github.com/maximbaz/dotfiles)

-----

### 12. **jordanisaacs/dotfiles** 💾

This configuration is known for its advanced server and desktop setups, including a stateless ZFS machine setup, showing the power of NixOS for managing complex systems.

  * **Key Features**: Demonstrates an "Erase Your Darlings" inspired stateless machine using `impermanence` and ZFS snapshots, and showcases a complete GTK/QT theming on Wayland.
  * **Repository Link**: [github.com/jordanisaacs/dotfiles](https://github.com/jordanisaacs/dotfiles)

-----

### 13. **sioodmy/dotfiles** ⚡

This is a popular configuration known for its unique "autistic design philosophy" which includes a focus on wrapping binaries, avoiding extra inputs, and not copying from others. It's a good example of an individualistic and highly optimized setup.

  * **Key Features**: A minimalist flake-based setup with a focus on fast evaluation times, uses `base16` for theming, and includes a full dev shell with a configured Neovim.
  * **Repository Link**: [github.com/sioodmy/dotfiles](https://github.com/sioodmy/dotfiles)

-----

### 14. **G-O-O-D/nix-config** 👨‍💻

This repository is known for its well-organized structure and includes configurations for multiple machines, making it a great resource for managing a diverse range of systems.

  * **Key Features**: Supports a variety of machines including a server, a desktop PC, and a laptop. It uses Nix flakes and is structured to be easily extensible.
  * **Repository Link**: [github.com/G-O-O-D/nix-config](https://www.google.com/search?q=https://github.com/G-O-O-D/nix-config)

-----

### 15. **matthewcroughan/nix-config** 🦀

This configuration is a good example of a modern, rust-focused development environment managed with Nix. It's a great resource for developers who want a fully declarative setup.

  * **Key Features**: A declarative development environment for Rust, Go, and other languages. It leverages Home Manager and is a good example of a developer-centric configuration.
  * **Repository Link**: [github.com/matthewcroughan/nix-config](https://www.google.com/search?q=https://github.com/matthewcroughan/nix-config)

-----

### 16. **gvolpe/nix-config** ⚙️

A comprehensive configuration by a prominent functional programmer, showcasing how to build a declarative, pure environment for development.

  * **Key Features**: Well-documented, supports multiple hosts, and focuses on a minimal yet powerful developer setup.
  * **Repository Link**: [github.com/gvolpe/nix-config](https://github.com/gvolpe/nix-config)

-----

### 17. **nix-community/home-manager** 🏡

While not a personal configuration, the Home Manager repository itself is a goldmine for example configurations from the community. Its `example` directory is a well-known resource for seeing various ways to use the tool.

  * **Key Features**: This is the primary repository for the Home Manager tool, and its examples showcase best practices for managing user-level packages and dotfiles.
  * **Repository Link**: [github.com/nix-community/home-manager](https://github.com/nix-community/home-manager)

-----

### 18. **lovesegfault/nix-config** 🖤

This configuration is a well-known example of a highly-customized setup with a focus on an elegant, functional desktop environment. It's particularly useful for those interested in a polished user experience.

  * **Key Features**: A comprehensive configuration for a full desktop setup, including theming and application-specific settings. It uses Home Manager and is known for its attention to detail.
  * **Repository Link**: [github.com/lovesegfault/nix-config](https://github.com/lovesegfault/nix-config)

-----

### 19. **nix-community/NixOS-starter-template** ✨

While not a personal dotfile repo, this template is one of the most popular and recommended starting points for new NixOS users. It's a goldmine of best practices and a foundational resource for anyone beginning their Nix journey.

  * **Key Features**: Provides a well-documented and minimal `flake.nix` that is easy to understand and extend. It demonstrates the use of Home Manager and secrets management, acting as a great educational tool.
  * **Repository Link**: [github.com/nix-community/NixOS-starter-template](https://www.google.com/search?q=https://github.com/nix-community/NixOS-starter-template)
