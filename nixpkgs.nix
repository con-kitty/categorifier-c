/*
NIXPKGS version
Any archive of nixpkgs can be used.
The simplest update solution is to look at
http://github.com/NixOS/nixpkgs and pick the latest commit for
the desired branch. The archive can then be fetched at:
https://github.com/NixOS/nixpkgs/archive/COMMIT_NUMBER.tar.gz;
and the control sum computed using `sha256`.
*/

let
  sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  version = "21.11";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/${version}.tar.gz";
})
