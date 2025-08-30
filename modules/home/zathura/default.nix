{ ... }: {
  programs.zathura = {
    enable = true;
    options = {
      font = "JetBrainsMono Nerd Font 12";
      adjust-open = "best-fit";
      selection-clipboard = "clipboard";
      window-title-basename = "true";
      pages-per-row = "1";
      scroll-page-aware = "true";
      scroll-full-overlap = "0.01";
      scroll-step = "100";
      smooth-scroll = true;
      zoom-min = "10";
      recolor = true;
      statusbar-h-padding = 10;
      statusbar-v-padding = 10;
      render-loading = true;
    };
  };
}
