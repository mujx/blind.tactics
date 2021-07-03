let
  pkgs = import (builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/42f23a740e980cdd33b85f83508c97092bbdea4a.tar.gz";
  }) { };

  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "b197e8764680f39416352f2ab3ed8f25e2a6b333";
    sha256 = "1b59dddrkdvh0i26any5g7lxxaxnn9af61dhxbb9bdb5n831dviw";
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
