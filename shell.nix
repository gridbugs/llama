{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    gcc
    pkgconfig
    gmp
    libffi
    alsaLib
  ];
}
