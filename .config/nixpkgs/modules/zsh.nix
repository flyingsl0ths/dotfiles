{ pkgs, lib, ... }:
with lib; {
  programs.zsh = {
    enable = true;

    enableSyntaxHighlighting = true;

    initExtra =
      let
        npmGlobalDir = "$HOME/.npm-global/bin";
      in
      ''
        export PATH=$PATH:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:${npmGlobalDir}
        export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels''${NIX_PATH:+:$NIX_PATH}

        PROMPT="%F{13}%f%F{14}%f%F{41}%f %F{214}  %f%F{252}%~%f "

        eval "$(direnv hook zsh)"

        setopt autocd
        # enable vi mode
        bindkey -v

        zstyle ':autocomplete:tab:*' widget-style menu-select
        zstyle ':autocomplete:*' min-input 2

        function cc() python3 -c "from math import *; print($*);"

        pokemon-colorscripts -r --no-title
      '';


    shellAliases = {
      py = "python3";
      pip_install = "pip3 install --user";
      bpy = "bpython";
      cc = "noglob cc";

      sql_login = "sudo mycli -u root";
      mariadb_srvc = "sudo systemctl start mariadb";
      st_mariadb_srvc = "sudo systemctl stop mariadb";

      cmb = "cmake --build";
      cmtg = "cmake --target";
      ccpp = "clang++ -std=c++17 -Wall -Werror -Wextra -Wsign-conversion";
      cmpc = "clang -std=c99 -Wall -Werror -Wextra -Wsign-conversion -pedantic";
      cmcc = "cmake -G Ninja -DCMAKE_C_COMPILER=clang";
      cmcpp = "cmake -G Ninja -DCMAKE_CXX_COMPILER=clang++";
      ctv = "ctest -VV";

      nd = "node";

      gdl = "gradle";
      gdw = "./gradlew -q";

      cgo = "cargo";

      cbl = "cabal";

      yt = "ytfzf -t -l";
      ytm = "ytfzf -m -t -l";

      ta = "tmux a -t";
      tls = "tmux ls";
      ts = "tmux new -s";
      tk = "tmux kill-session";
      #tmux = "TERM=screen-256color-bce tmux";

      ld = "cd -";
      ls = "exa --icons -1";
      la = "exa --icons -1 --all";
      ll = "exa --icons -1 -l";
      lhi = "exa --icons -1 -l -i";

      mkdir = "mkdir -p";
      rmdir = "rm -r";

      pf = "paste_file";
      sp = "scratchpad";
      fm = "ranger";
      xtr = "tar xf";
      code = "vscodium";
      n = "nvim";
      v = "TERM=xterm-256color vim";
      zth = "zathura";
      valgrind = "valgrind --leak-check=full";
      cat = "bat --theme 1337";
      ctags = "ctags --append=yes";
      ugzp = "gzip --uncompress";
      mtp = "jmtpfs -o auto_unmount";
      paste = "xclip -i -sel c";
      ps = "pipes.sh";

      xmr = "xmonad --recompile && xmonad --restart";
      mic = "sudo make install && sudo make clean";

      vrc = "v ~/.vimrc";
      vss = "v ~/.vimscripts";
      ncd = "nvim  ~/.config/nvim";
      gtc = "nnvim ~/.gitconfig";
      i3c = "nvim ~/.config/i3/config";
      xmc = "nvim ~/.xmonad/xmonad.hs";

      q = "exit";
      of = "xdg-open";
      clr = "clear";
      please = "sudo";
      rm = "rm -r";
      df = "df -h";
      mv = "mv -i";
      kjb = "kill -KILL";
      imv = "nsxiv -q";
      walls = "cd ~/.local/share/wallpaper/";
      wfo = "nmcli radio wifi on";
      wff = "nmcli radio wifi off";
      krln = "uname -rs";
      h = "history";
      hs = "history | rg";
      hsi = "history | rg -s";
      gp = "git pull";

      # NixOs Specific
      dcfgs = "sudo nix-env --delete-generations old --profile /nix/var/nix/profiles/system";
      ugrb = "sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch";
      ucfg = "sudo nixos-rebuild switch";
      ocfg = "sudo nvim /etc/nixos/configuration.nix";
      nxs = "nix-shell";
      hms = "home-manager switch";
    };

    history = {
      save = 1000;
      size = 1000;
      path = ''${builtins.getEnv "HOME"}/.zsh_history'';
    };

    plugins = [
      {
        name = "zsh-autocomplete";
        src = pkgs.zsh-autocomplete;
        file = "share/zsh-autocomplete/zsh-autocomplete.plugin.zsh";
      }
    ];

  };
}
