with (import ./default.nix {});

app.env.overrideAttrs (drv: with pkgs.haskellPackages; {
  shellHook = ''
    export PATH=$PATH:${cabal-install}/bin/
    function build () {
      cabal build miso-from-html
    }
    function clean () {
      cabal clean
    }
    function hlint () {
      ${pkgs.hlint}/bin/hlint .
    }
    function docs () {
      cabal haddock
    }
    function parse () {
      cabal run miso-from-html
    }
    function repl () {
      cabal repl miso-from-html
    }
  '';
})
