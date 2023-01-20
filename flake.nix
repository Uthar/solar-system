{
  description = "Flake utils demo";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-cl.url = "github:uthar/nix-cl";

  outputs = { self, nixpkgs, flake-utils, nix-cl }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        sbcl = nix-cl.packages.${system}.sbcl;
      in
      {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [
              (sbcl.withPackages (ps: with ps; [
                alexandria
                _3d-quaternions
                cl-opengl
                sdl2
              ]))
            ];
          };
        };
      }
    );
}
