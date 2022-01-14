let
  pkgs = import (builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  }) { };

  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "678070816270726e2f428da873fe3f2736201f42";
    sha256 = "13l9c1sgakpmh9f23201s8d1lnv0zz0q1wsr1lc92wdpkxs9nii4";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "ui";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    pursPkgs.purs-tidy
    pursPkgs.purescript-language-server
    pkgs.nodejs
    pkgs.cacert
    pkgs.yarn
  ];
}
