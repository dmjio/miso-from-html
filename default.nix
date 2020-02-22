with (builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json));
{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { config.allowUnfree = true; }
}:
let
  app = pkgs.haskellPackages.callCabal2nix "miso-from-html" ./. {};
in
{
  inherit app pkgs;
}
