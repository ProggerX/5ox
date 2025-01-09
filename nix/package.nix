{ pkgs, isShell ? false}:
pkgs.haskell.packages.ghc984.developPackage {
	root = ../.;
	returnShellEnv = isShell;
}
