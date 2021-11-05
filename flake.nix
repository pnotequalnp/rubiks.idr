{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    idris2-pkgs.url = "github:claymager/idris2-pkgs/idrisPackage";
  };

  outputs = { self, nixpkgs, idris2-pkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" "i686-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ idris2-pkgs.overlay ]; };
        inherit (pkgs.idris2-pkgs._builders) idrisPackage devEnv;
        rubiks = idrisPackage ./. { };
        test = idrisPackage ./test { extraPkgs.rubiks = rubiks; };
      in
      {
        defaultPackage = rubiks;

        packages = { inherit rubiks test; };

        devShell = pkgs.mkShell {
          buildInputs = [ (devEnv rubiks) ];
        };
      }
    );
}
