{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.initrd.kernelModules = [ "amdgpu" ];

  networking.hostName = "nixos"; # Define your hostname.

  networking.extraHosts =
    let
       myHostFile = pkgs.fetchurl {
       url = "https://block.energized.pro/basic/formats/hosts.txt";
       sha256 = "sha256-sPM2XLs2CrLRN8YW54r6Bvzu1PtI/YVIkxlo9Qx2r9w=";
       };
    in
    '' ${builtins.readFile myHostFile} '';

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Mexico_City";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  nixpkgs.overlays = [
    (final: prev: {
      dwm = prev.dwm.overrideAttrs (old: { src = /home/flyingsloths/Documents/code/dwm-fork ;});
      st =  prev.st.overrideAttrs (old: { buildInputs = old.buildInputs ++ [ pkgs.harfbuzz.dev ]; src = /home/flyingsloths/Documents/code/st-fork ;});
      picom = prev.picom.overrideAttrs (old: { src = /home/flyingsloths/Documents/code/picom ;});
    })
  ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" ];

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.gnome.core-utilities.enable = false;

  # Enable the i3 window manager
  services.xserver.windowManager.i3.package = pkgs.i3-gaps; 
  services.xserver.windowManager.i3.enable = true;

  services.xserver.windowManager.awesome.enable = true;

  services.xserver.windowManager.bspwm.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.dwm.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = false;

  hardware.opengl.enable = true;

  hardware.opengl.extraPackages = with pkgs; [
    rocm-opencl-icd
    rocm-opencl-runtime
    amdvlk
  ];

  environment.variables.AMD_VULKAN_ICD = "RADV";

  hardware.opengl.driSupport = true;

  # For 32 bit applications
  hardware.opengl.driSupport32Bit = true;

  # For 32 bit applications 
  hardware.opengl.extraPackages32 = with pkgs; [
    driversi686Linux.amdvlk
  ];

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.flyingsloths = {
    isNormalUser = true;
    description = "flyingsloths";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  programs.bash.enableCompletion = true;
  programs.zsh.enable = true;
  users.users.flyingsloths.shell = pkgs.zsh;

  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # Tools
    wget
    lazygit
    broot
    xwallpaper
    pamixer
    brightnessctl
    sxhkd
    ytfzf
    unzip
    fd
    ripgrep
    exa
    bat
    ryzenadj
    picom
    wine64
    mono
    dunst
    polybarFull
    xsettingsd
    redshift
    xautolock
    fzf
    ueberzug
    xclip
    bottles
    direnv

    # Theming-related
    capitaine-cursors
    papirus-icon-theme
    catppuccin-gtk
    material-design-icons
    font-awesome_5

    # Development
    st
    tmux
    helix
    ranger
    nodejs
    lldb
    rustup
    rust-analyzer
    lua5_4
    jdk
    kotlin
    kotlin-language-server
    gradle
    sumneko-lua-language-server
    clang_14
    clang-tools_14
    cmake
    ninja
    haskell.compiler.ghc8107
    haskell-language-server
    hlint
    haskellPackages.brittany
    haskellPackages.hls-brittany-plugin
    cabal-install
    stack
    python310
    python310Packages.pip
    vscodium-fhs
    vim
    neovim
    git
    gnumake

    # Apps
    librewolf
    brave
    marktext
    pcmanfm
    lxappearance
    yuzu-mainline
    dmenu
    rofi-wayland
    i3lock-color
    emojipick
    networkmanager_dmenu
    zathura
    xfce.xfce4-taskmanager
    freshfetch
    mpv
    nsxiv
    gnome.gnome-tweaks
    lxde.lxrandr
    mate.mate-polkit
    networkmanagerapplet
    mgba
    eww-wayland
    melonDS
    dolphin-emu
  ];

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" "JetBrainsMono" ]; })
  ];

  # List services that you want to enable:

  networking.firewall.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leavecatenate(variables, "bootdev", bootdev)
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
