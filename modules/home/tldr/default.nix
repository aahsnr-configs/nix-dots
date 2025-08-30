# ~/.config/home-manager/tldr/default.nix
{ ... }: {
  programs.tealdeer = {
    enable = true;
    settings = {
      display = {
        compact = true;
        use_pager = true;
      };
      style = {
        description.color = "#cad3f5";
        command_name = {
          color = "#c6a0f6";
          style = "bold";
        };
        example_text.color = "#cad3f5";
        example_code.color = "#a6da95";
        example_variable = {
          color = "#ed8796";
          style = "italic";
        };
      };
      updates.auto_update = true;
    };
  };
}
