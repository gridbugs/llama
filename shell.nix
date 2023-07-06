{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    gcc
    pkgconfig
    gmp
    libffi
    alsaLib
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
    xorg.libXScrnSaver
    SDL2
  ];
}
