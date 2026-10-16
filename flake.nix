{
  description = "simplang-haskell の開発環境（ormolu の版を固定する。GHC・cabal・HLS は ghcup で管理する）";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      # エディタ（HLS の external 設定）・Claude Code の hook・CI が、この ormolu を共通で使う（docs/step027.md）
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = [ pkgs.ormolu ];
        };
      });
    };
}
