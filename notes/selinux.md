An Updated Guide to Setting Up SELinux in NixOS (August 2025)

### A Modern, Experimental Approach to SELinux on NixOS

For years, integrating the robust, fine-grained access control of Security-Enhanced Linux (SELinux) into NixOS was considered impractical. However, thanks to significant community efforts in 2025, basic SELinux support is now an experimental feature in NixOS. This guide provides the most up-to-date instructions for enabling and configuring it.

**Crucial Disclaimer:** SELinux support in NixOS is highly experimental and should not be used in production environments. The underlying mechanisms are still evolving, and you may encounter significant issues. For a stable and officially supported Mandatory Access Control (MAC) system on NixOS, AppArmor is the recommended choice.

#### The Breakthrough: How SELinux Became Possible

The primary obstacle for SELinux on NixOS has always been the immutability of the `/nix/store`. SELinux works by attaching security labels as extended attributes to files and directories, a practice fundamentally incompatible with the Nix model, which treats store paths as read-only.

Recent progress, largely driven by community member Tristan Ross's work on the ExpidusOS project, has introduced a new approach. This implementation enables SELinux by focusing on process confinement and labeling parts of the filesystem outside the `/nix/store`, while introducing a new NixOS module and a `nixpkgs` flag to build packages with SELinux awareness. The work culminated in a series of pull requests to integrate basic SELinux support into the official Nixpkgs repository.

#### Configuration: Enabling SELinux on Your System

Enabling SELinux requires editing your NixOS configuration to use the new module, activate the necessary kernel parameters, and select a security policy.

**Step 1: Enable the SELinux Module and Build-time Support**

First, you must enable the `security.selinux` module in your `configuration.nix`. Additionally, you need to set the `selinuxSupport` flag in your `nixpkgs` configuration. This flag ensures that core packages are rebuilt with SELinux support where available.

```nix
{ config, pkgs, ... }:

{
  # Enable SELinux support for building packages
  nixpkgs.config.selinuxSupport = true;

  # Enable the main SELinux module
  security.selinux.enable = true;
}
```

**Step 2: Configure the Kernel**

The Linux kernel requires a specific boot parameter to activate SELinux as the active security module. The NixOS module automatically adds a kernel patch to ensure the required `CONFIG_SECURITY_SELINUX` options are enabled.

```nix
{ config, pkgs, ... }:

{
  # ... previous configuration

  # Add the kernel parameter to activate SELinux at boot
  boot.kernelParams = [ "security=selinux" ];
}
```

**Step 3: Set the SELinux Mode and Policy**

For initial setup, it is strongly recommended to run SELinux in `permissive` mode. In this state, SELinux logs policy violations without actually blocking any actions, allowing you to troubleshoot and develop your policy without breaking your system. Once you are confident in your policy, you can switch to `enforcing` mode.

The module also requires you to specify a policy version, which must match the version supported by your system's kernel.

```nix
{ config, pkgs, ... }:

{
  # ... previous configuration

  # Set SELinux to permissive mode (recommended for initial setup)
  # Change to `true` to enforce policies.
  security.selinux.enforcing = false;

  # Specify the policy version to use. This must align with the
  # version supported by your kernel's SELinux implementation.
  security.selinux.policyVersion = 33;
}
```

### Complete Example `configuration.nix`

This example combines all the necessary options for a basic SELinux setup.

```nix
{ config, pkgs, ... }:

{
  # 1. Enable SELinux support for packages in nixpkgs.
  # This flag triggers packages like coreutils and systemd
  # to be compiled with SELinux awareness.
  nixpkgs.config.selinuxSupport = true;

  # 2. Enable the main SELinux module.
  # This handles installing necessary tools (like policycoreutils),
  # configuring the SELinux policy, and patching the kernel.
  security.selinux.enable = true;

  # 3. Set the SELinux operational mode.
  # `false` (permissive) logs violations without blocking them.
  # This is essential for initial setup and policy refinement.
  # Set to `true` (enforcing) for production use.
  security.selinux.enforcing = false;

  # 4. Define the SELinux policy version.
  # The version of the reference policy to build and install.
  # This was a key factor in getting a working policy loaded,
  # as the userspace tools must target the same version as the kernel.
  security.selinux.policyVersion = 33; # Adjust if your kernel requires a different version

  # 5. Add the necessary kernel boot parameter.
  # This instructs the kernel to use SELinux as its security module.
  boot.kernelParams = [ "security=selinux" ];

  # 6. Explicitly add SELinux utilities to the system path.
  # While the module manages dependencies, adding these ensures they
  # are readily available for administrative tasks.
  environment.systemPackages = with pkgs; [
    libselinux
    policycoreutils
  ];
}
```

After adding this to your configuration, rebuild your system:

```sh
sudo nixos-rebuild switch
```

### Verifying the SELinux Status

After a reboot, you can check if SELinux is active and what mode it is in by running `sestatus`:

```sh
sestatus
```

The output should show `SELinux status: enabled`, the current mode (e.g., `Permissive`), and the loaded policy name (e.g., `refpolicy`).

### Current Limitations and Future Work

The fundamental challenge of the immutable `/nix/store` remains. The current implementation does not label files within the Nix store. This means that while SELinux can confine processes and protect non-store paths like `/etc` and `/var`, it cannot enforce policies based on the labels of the binaries themselves if they reside in `/nix/store`.

The community is exploring potential long-term solutions, such as using an overlay filesystem on top of the Nix store to apply labels without modifying the original store paths.

### A Note on Determinate Nix

You may encounter the "Determinate Nix for Linux with SELinux" installer. It is important to understand that this tool is designed to allow the **Nix package manager to run on other SELinux-enabled distributions** like Fedora or RHEL. It provides SELinux policies for the Nix daemon's operations on those systems; it does not enable SELinux within a NixOS installation.
