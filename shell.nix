let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-25.05";
  pkgs = import nixpkgs { config = {}; };
in

pkgs.mkShellNoCC {
  packages = with pkgs; [
    gcc13
    jdk
    scala_2_13
    sbt
    verilator
    zlib
  ];

  greeting = "Welcome to the HADES nix-shell";

  shellHook = ''echo $greeting'';
}