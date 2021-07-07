let
  pkgs = import (builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
  }) { };

  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "47bdc016c7d56e987ca1aca690b1d6c9816a8584";
    sha256 = "051fzxd03y0c63sll2bhn0h66dywy9lw6ylyh5vq8fymvix20q94";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "ui";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    pkgs.yarn
    pkgs.nodePackages.purty
    pkgs.nodePackages.purescript-language-server
    pkgs.nodejs
    pkgs.git
  ];
}
