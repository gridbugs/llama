{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    gcc
    pkg-config
    gmp
    libffi
    alsa-lib
    libao
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
    xorg.libXScrnSaver
    SDL2
  ];
}
