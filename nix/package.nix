{ pkgs, isShell ? false}:
pkgs.haskellPackages.developPackage { root = ../.; returnShellEnv = isShell; }
