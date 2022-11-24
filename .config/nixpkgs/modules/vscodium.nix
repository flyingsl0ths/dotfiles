{ pkgs, lib, ... }:
with lib; {
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      arrterian.nix-env-selector
      asvetliakov.vscode-neovim
      editorconfig.editorconfig
      esbenp.prettier-vscode
      foxundermoon.shell-format
      haskell.haskell
      jkillian.custom-local-formatters
      jnoortheen.nix-ide
      justusadam.language-haskell
      llvm-vs-code-extensions.vscode-clangd
      mads-hartmann.bash-ide-vscode
      matklad.rust-analyzer
      ms-python.python
      pkief.material-icon-theme
      pkief.material-product-icons
      redhat.java
      ritwickdey.liveserver
      timonwong.shellcheck
      vadimcn.vscode-lldb
      xaver.clang-format
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "autodocstring";
        publisher = "njpwerner";
        sha256 = "sha256-NI0cbjsZPW8n6qRTRKoqznSDhLZRUguP7Sa/d0feeoc=";
        version = "0.6.1";
      }

      {
        name = "clang-tidy";
        publisher = "notskm";
        sha256 = "sha256-neAvG8bk8yzpbuSzvVVi8Z3lCr29FBncXx3Sv/KChHw=";
        version = "0.5.1";
      }

      {
        name = "cmake";
        publisher = "twxs";
        sha256 = "sha256-CFiva1AO/oHpszbpd7lLtDzbv1Yi55yQOQPP/kCTH4Y=";
        version = "0.0.17";
      }

      {
        name = "cmake-tools";
        publisher = "ms-vscode";
        sha256 = "sha256-lJm/unw6zIHPONEvM+WzTIeFMn+3lJG+0/ghkh1JU9E=";
        version = "1.13.8";
      }

      {
        name = "cmake-format";
        publisher = "cheshirekow";
        sha256 = "sha256-NdU8J0rkrH5dFcLs8p4n/j2VpSP/X7eSz2j4CMDiYJM=";
        version = "0.6.11";
      }

      {
        name = "direnv";
        publisher = "mkhl";
        sha256 = "sha256-5/Tqpn/7byl+z2ATflgKV1+rhdqj+XMEZNbGwDmGwLQ=";
        version = "0.6.1";
      }

      {
        name = "kotlin";
        publisher = "fwcd";
        sha256 = "sha256-djo1m0myIpEqz/jGyaUS2OROGnafY7YOI5T1sEneIK8=";
        version = "0.2.26";
      }

      {
        name = "vscode-java-debug";
        publisher = "vscjava";
        sha256 = "sha256-HjcafkFTUOD6NlPRJeQxm4UWSAwlpNrU7lRJw8PRPQ4=";
        version = "0.44.2022092202";
      }

      {
        name = "lua";
        publisher = "sumneko";
        sha256 = "sha256-Unzs9rX/0MlQprSvScdBCCFMeLCaGzWsMbcFqSKY2XY=";
        version = "3.5.6";
      }

      {
        name = "local-lua-debugger-vscode";
        publisher = "tomblind";
        sha256 = "sha256-7uZHbhOa/GT9F7+xikaxuQXIGzre1q1uWTWaTJhi2UA=";
        version = "0.3.3";
      }

      {
        name = "vsc-python-indent";
        publisher = "KevinRose";
        sha256 = "sha256-ISZzHxDdy1dArDWC3xwdnHZGWbNzmwspie8Le88tbpM=";
        version = "1.17.0";
      }

      {
        name = "sass-indented";
        publisher = "Syler";
        sha256 = "sha256-i1z9WTwCuKrfU4AhdoSvGEunkk8gdStsod8jHTEnoFY=";
        version = "1.8.22";
      }

      {
        name = "catppuccin-vsc";
        publisher = "Catppuccin";
        sha256 = "sha256-cJQCd1/U3wWS+LGqHGjla9VDGrX4SzOM+/hXZOu3r3Q=";
        version = "2.1.1";
      }
    ];
  };
}
