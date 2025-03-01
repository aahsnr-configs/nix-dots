{ config, pkgs, ... }:

{
  programs.starship = {
    enable = true;
    enableTransience = true;
    enableZshIntegration = true;
    settings = {
      add_newline = false;
      character = {
        success_symbol = " ";
        error_symbol = " ";
        vicmd_symbol = " ";
      };
      command_timeout = 1000;
      directory = {
        home_symbol = "~ ";
        style = "cyan";
      };
      git_commit.tag_symbol = " tag ";
      git_branch = {
        # style = "purple";
        symbol = " ";
      };
      git_metrics = {
        added_style = "[]bold green";
        deleted_style = "bold red";
        disabled = true;
      };
      hostname = {
        ssh_only = true;
        format = "[$hostname] ";
        disabled = false;
      };
      line_break.disabled = true;
      scan_timeout = 10000;
      format = "$directory$git_branch$git_metrics$git_commit$git_state$git_status$all";

      bun.symbol = " ";
      c.symbol = " ";
      conda.symbol = " ";
      cmake.symbol = " ";
      directory.read_only = " ro";
      docker_context.symbol = " ";
      lua.symbol = "󰢱 ";
      memory_usage.symbol = "󰍛 ";
      nodejs.symbol = " ";
      package.symbol = " ";
      perl.symbol = " ";
      python.symbol = " ";
      rust.symbol = " ";
      sudo.symbol = " ";
    };
  };
}
