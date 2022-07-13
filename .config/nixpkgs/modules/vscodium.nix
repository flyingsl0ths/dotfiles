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
      jnoortheen.nix-ide
      justusadam.language-haskell
      llvm-vs-code-extensions.vscode-clangd
      mads-hartmann.bash-ide-vscode
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
        sha256 = "sha256-ixs1YOxXWNkhFFW5luJxqzgKcs4ZbkxlECnjWNeYFk8=";
        version = "1.12.12";
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
        sha256 = "sha256-o/8L1AZ6yz5EOYWa7RIFcuTeTItWmiV/nSPOLjsIoqo=";
        version = "0.42.2022062902";
      }

      {
        name = "lua";
        publisher = "sumneko";
        sha256 = "sha256-jqZKD2MQW17an6JzZST5Dbxn1wbkkS/XHgWg78ylWvU=";
        version = "3.4.2";
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
        name = "rust";
        publisher = "rust-lang";
        sha256 = "sha256-Y33agSNMVmaVCQdYd5mzwjiK5JTZTtzTkmSGTQrSNg0=";
        version = "0.7.8";
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
        sha256 = "sha256-4M8y8dc8BpH1yhabYJsHDT9uDWeqYjnvPBgLS+lTa5I=";
        version = "1.0.6";
      }
    ];
  };
}
