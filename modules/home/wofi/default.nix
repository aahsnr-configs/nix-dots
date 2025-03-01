{ ... }:

{
  programs.wofi = {
    enable = true;
    settings = {

      ## General
      show = "drun";
      prompt = "Apps";
      normal_window = true;
      layer = "top";
      term = "alacritty";

      ## Geometry
      width = 600;
      height = 340;
      location = 0;
      orientation = "vertical";
      halign = "fill";
      line_wrap = "off";
      dynamic_lines = false;

      ## Images
      allow_markup = true;
      allow_images = true;
      image_size = 24;

      ## Search
      exec_search = false;
      hide_search = false;
      parse_search = false;
      insensitive = false;

      ## Other
      hide_scroll = true;
      no_actions = true;
      sort_order = "default";
      gtk_dark = true;
      filter_rate = 100;

      ## Keys
      key_expand = "Tab";
      key_exit = "Escape";
    };

    style = ''
    /** ********** Colors ********** **/
    @define-color background      #1E1E2E;
    @define-color background-alt1 #28283d;
    @define-color background-alt2 #32324d;
    @define-color foreground      #CDD6F4;
    @define-color selected        #89B4FA;
    @define-color black           #45475A;
    @define-color red             #F38BA8;
    @define-color green           #A6E3A1;
    @define-color yellow          #F9E2AF;
    @define-color blue            #89B4FA;
    @define-color magenta         #F5C2E7;
    @define-color cyan            #94E2D5;
    @define-color white           #BAC2DE;

    /** ********** Fonts ********** **
    * {
      font-family: "JetBrainsMono Nerd Font";
      font-size: 12px;
    }

    #window {
      background-color: @background;
	    color: @foreground;
	    border: 0px solid @background-alt1;
      border-radius: 0px;
    }

    #outer-box {
	    padding: 10px;
    }

    #input {
	    background-color: @background-alt1;
	    border: 0px solid @background-alt2;
	    padding: 4px 12px;
    }

    #scroll {
	    margin-top: 10px;
    }

    #inner-box {
    }

    #img {
	    padding-right: 8px;
    }

    #text {
	    color: @foreground;
    }

    #text:selected {
	    color: @background;
    }

    #entry {
	    padding: 6px;
    }

    #entry:selected {
	    background-color: @selected;
	    color: @background;
    }

    #unselected {
    }

    #selected {
    }

    #input, #entry:selected {
  	  border-radius: 12px;
    }
    '';

  };
}
