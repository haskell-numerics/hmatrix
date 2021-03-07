let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
      };
    };
};

nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
    sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
};

in

{ pkgs ? import nixpkgs { overlays = [ myHaskellPackageOverlay ]; } }:

let

haskellPackages = pkgs.myHaskellPackages;

in

haskellPackages.callPackage ./default.nix {
}
