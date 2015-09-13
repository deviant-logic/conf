{ pkgs }: {
  allowBroken = true;
  allowUnfree = true;

  haskellPackageOverrides = with pkgs.haskell.lib; self: super: {
    lens = dontCheck super.lens;
    mkDerivation = expr: super.mkDerivation (expr // { enableLibraryProfiling = true; });
  };
}
