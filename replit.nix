{ pkgs }: {
    deps = [
		pkgs.nodePackages.prettier
        pkgs.haskellPackages.ghc
        pkgs.haskell-language-server
    ];
}