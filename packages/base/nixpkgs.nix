# Pin the nixpkgs version to a specific commit for reproducibility.
let pkgs =  (import <nixpkgs> {}).fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "caa81f7bcf5dbb8cee441443ba8225ebae8b4b2c";
      sha256 = "04530jl2chy0klima5mby3pqq2xwgnm3mhv7gxzc4wa5s6i42q6h";
      };
 in import pkgs {}
