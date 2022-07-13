{ config, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  #nixpkgs.config.allowUnfree = true;

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
  time.timeZone = "";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  nixpkgs.overlays = [
    (final: prev: {
      dwm = prev.dwm.overrideAttrs (old: {
        src = pkgs.fetchgit {
          url = "https://github.com/flyingsl0ths/dwm-fork.git";
          rev = "3e4a5a4f8e181c312132d80880072a8357a9d16e";
          sha256 = "sha256-SI8LMmurS0LPJzfwn/hz3+1iwsKgd4zkwLvuF2rickY=";
        };
      });

      st = prev.st.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [ pkgs.harfbuzz.dev ];
        src = pkgs.fetchgit {
          url = "https://github.com/flyingsl0ths/st-fork.git";
          rev = "6f874d56315eb58ed3197a3f75726d7648982317";
          sha256 = "sha256-ffRk89JE08rGzhNW9MF9DGdrE6oJ8cshdNhBi7xXpUw=";
        };
      });

      dmenu = prev.dmenu.overrideAttrs (old: {
        src = pkgs.fetchgit {
          url = "https://github.com/flyingsl0ths/dmenu-fork.git";
          rev = "8470bdbf7861b765441069129f0a350f01430786";
          sha256 = "sha256-4j0pPrly07YEEcTtB2+ZoSxVf93TTr76F43d5a/rEP4=";
        };
      });

      picom = prev.picom.overrideAttrs (old: {
        src = pkgs.fetchgit {
          url = "https://github.com/pijulius/picom.git";
          rev = "982bb43e5d4116f1a37a0bde01c9bda0b88705b9";
          sha256 = "sha256-YiuLScDV9UfgI1MiYRtjgRkJ0VuA1TExATA2nJSJMhM=";
        };
      });
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
  services.xserver.windowManager.i3 =
    {
      enable = true;
      package = pkgs.i3-gaps;
    };

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

  environment.variables.AMD_VULKAN_ICD = "RADV";
  hardware.opengl.enable = true;
  hardware.opengl.setLdLibraryPath = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.opengl.extraPackages = with pkgs; [
    rocm-opencl-icd
    rocm-opencl-runtime
    amdvlk
  ];

  # For 32 bit applications 
  hardware.opengl.extraPackages32 = with pkgs; [
    driversi686Linux.amdvlk
    pkgsi686Linux.libva
  ];

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  hardware.pulseaudio.support32Bit = true;
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

  users.users.flyingsloths.packages = with pkgs;
    [
      vulkan-tools
    ];

  programs.bash.enableCompletion = true;
  users.users.flyingsloths.shell = pkgs.zsh;

  qt5.enable = true;
  qt5.platformTheme = "gtk2";
  qt5.style = "gtk2";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # Tools
    bat
    brightnessctl
    broot
    direnv
    dunst
    exa
    fd
    fzf
    lazygit
    pamixer
    picom
    polybarFull
    redshift
    ripgrep
    ryzenadj
    sxhkd
    ueberzug
    unzip
    wget
    wineWowPackages.stable
    xautolock
    xclip
    xsettingsd
    xwallpaper
    ytfzf

    # Theming-related
    font-awesome_5
    material-design-icons
    qt5ct

    # Development
    #android-studio
    cabal-install
    clang-tools_14
    cmake
    cmake-language-server
    git
    gnumake
    gradle
    haskell.compiler.ghc8107
    haskell-language-server
    helix
    hlint
    jdk
    kotlin
    kotlin-language-server
    lldb_14
    llvmPackages_14.clang
    llvmPackages_14.libcxxStdenv
    lua5_4
    neovim
    ninja
    nixpkgs-fmt
    nodejs
    nodePackages.bash-language-server
    nodePackages.npm-check-updates
    nodePackages.typescript
    python310
    python310Packages.black
    python310Packages.bpython
    python310Packages.mypy
    python310Packages.pip
    python310Packages.pytest
    ranger
    rust-analyzer
    rustup
    shellcheck
    shfmt
    st
    stack
    sumneko-lua-language-server
    tmux
    vim

    # Apps
    brave
    dmenu
    dolphin-emu
    emojipick
    eww-wayland
    freshfetch
    gimp
    gnome.gnome-tweaks
    i3lock-color
    librewolf
    lxappearance
    lxde.lxrandr
    marktext
    mate.mate-polkit
    melonDS
    mgba
    mpv
    networkmanagerapplet
    networkmanager_dmenu
    noto-fonts-extra
    nsxiv
    pcmanfm
    rofi-wayland
    xfce.xfce4-taskmanager
    yuzu-mainline
    zathura
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
