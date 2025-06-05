{
  inputs = { utils.url = "github:numtide/flake-utils"; };
  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            coreutils
            (haskellPackages.ghcWithPackages
              (ps: with ps; [ conduit conduit-extra ]))
            ghcid
            haskell-language-server
          ];
        };
      });
}
