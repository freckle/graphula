{
  inputs = {
    stable.url = "github:nixos/nixpkgs/nixos-23.11";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    freckle.url = "github:freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgsArgs = { inherit system; config = { }; };

        nixpkgs = {
          stable = import inputs.stable nixpkgsArgs;
          unstable = import inputs.unstable nixpkgsArgs;
        };
        freckle = inputs.freckle.packages.${system};
        freckleLib = inputs.freckle.lib.${system};

      in
      rec {
        packages = {
          cabal = nixpkgs.stable.cabal-install;

          fourmolu = freckle.fourmolu-0-13-x;

          ghc = freckleLib.haskellBundle {
            ghcVersion = "ghc-9-4-6";
            packageSelection = p: [
              p.annotated-exception
              p.case-insensitive
              p.http-client
              p.http-client-tls
              p.hspec
              p.iproute
              p.network
              p.QuickCheck
              p.unliftio
              p.wai
              p.warp
              p.yesod-core
            ];
          };

          haskell-language-server =
            nixpkgs.stable.haskell-language-server.override
              { supportedGhcVersions = [ "946" ]; };

          stack = nixpkgs.stable.writeShellApplication {
            name = "stack";
            text = ''
              ${nixpkgs.stable.stack}/bin/stack --system-ghc --no-nix "$@"
            '';
          };
        };

        devShells.default = nixpkgs.stable.mkShell {
          buildInputs = with (nixpkgs.stable); [
            pcre
            pcre.dev
            zlib
            zlib.dev
          ];

          nativeBuildInputs = with (packages); [
            cabal
            fourmolu
            ghc
            haskell-language-server
            stack
          ];

          shellHook = ''
            export STACK_YAML=stack.yaml
          '';
        };
      });
}
