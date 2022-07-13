{ config, pkgs, ... }:

{
  imports =
    [
      modules/zsh.nix
      modules/vscodium.nix
    ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "flyingsloths";
  home.homeDirectory = "/home/flyingsloths";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  gtk = {
    enable = true;

    font = {
      name = "FreeSans 13";
      package = pkgs.noto-fonts-extra;
    };

    theme = {
      name = "Catppuccin-teal-dark";
      package = pkgs.catppuccin-gtk;
    };

    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };

    cursorTheme = {
      name = "capitaine-cursors";
      package = pkgs.capitaine-cursors;
    };
  };
}
