{ lib, ... }:
with lib; {
  boot.kernel.sysctl = {
    # Hide kptrs even for processes with CAP_SYSLOG
    "kernel.kptr_restrict" = mkOverride 500 2;

    # Disable bpf() JIT (to eliminate spray attacks)
    "net.core.bpf_jit_enable" = mkDefault false;

    # The Magic SysRq key is a key combo that allows users connected to the
    # system console of a Linux kernel to perform some low-level commands.
    # Disable it, since we don't need it, and is a potential security concern.
    "kernel.sysrq" = mkDefault "0";

    # Disable ftrace debugging
    "kernel.ftrace_enabled" = mkDefault false;

    ## TCP hardening
    # Prevent bogus ICMP errors from filling up logs.
    "net.ipv4.icmp_ignore_bogus_error_responses" = mkDefault "1";
    # Reverse path filtering causes the kernel to do source validation of
    # packets received from all interfaces. This can mitigate IP spoofing.
    "net.ipv4.conf.default.rp_filter" = mkDefault "1";
    "net.ipv4.conf.all.rp_filter" = mkDefault "1";
    # Do not accept IP source route packets (we're not a router)
    "net.ipv4.conf.all.accept_source_route" = mkDefault "0";
    "net.ipv6.conf.all.accept_source_route" = mkDefault "0";
    # Don't send ICMP redirects (again, we're not a router)
    "net.ipv4.conf.all.send_redirects" = mkDefault "0";
    "net.ipv4.conf.default.send_redirects" = mkDefault "0";
    # Refuse ICMP redirects (MITM mitigations)
    "net.ipv4.conf.all.accept_redirects" = mkDefault "0";
    "net.ipv4.conf.default.accept_redirects" = mkDefault "0";
    "net.ipv4.conf.all.secure_redirects" = mkDefault "0";
    "net.ipv4.conf.default.secure_redirects" = mkDefault "0";
    "net.ipv6.conf.all.accept_redirects" = mkDefault "0";
    "net.ipv6.conf.default.accept_redirects" = mkDefault "0";
    # Protects against SYN flood attacks
    "net.ipv4.tcp_syncookies" = mkDefault "1";
    # Incomplete protection again TIME-WAIT assassination
    "net.ipv4.tcp_rfc1337" = mkDefault "1";

    ## TCP optimization
    # TCP Fast Open is a TCP extension that reduces network latency by packing
    # data in the sender’s initial TCP SYN. Setting 3 = enable TCP Fast Open for
    # both incoming and outgoing connections:
    "net.ipv4.tcp_fastopen" = mkDefault "3";
    # Bufferbloat mitigations + slight improvement in throughput & latency
    "net.ipv4.tcp_congestion_control" = mkDefault "bbr";
    "net.core.default_qdisc" = mkDefault "cake";
    "vm.max_map_count" = mkForce "2147483642";
  };

  boot.blacklistedKernelModules = [
    # Obscure network protocols
    "ax25"
    "netrom"
    "rose"

    # Old or rare or insufficiently audited filesystems
    "adfs"
    "affs"
    "bfs"
    "befs"
    "cramfs"
    "efs"
    "erofs"
    "exofs"
    "freevxfs"
    "f2fs"
    "hfs"
    "hpfs"
    "jfs"
    "minix"
    "nilfs2"
    "ntfs"
    "omfs"
    "qnx4"
    "qnx6"
    "sysv"
    "ufs"
  ];
}
