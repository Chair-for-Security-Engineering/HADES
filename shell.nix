let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.05";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShellNoCC {
  packages = with pkgs; [
    gcc
    jdk
    scala_2_13
    sbt
    verilator
  ];

  greeting = "Welcome to the HADES nix-shell";

  shellHook = ''echo $greeting'';
}