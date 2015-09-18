{ pkgs }: {
  allowBroken = true;
  allowUnfree = true;

  haskellPackageOverrides = with pkgs.haskell.lib; self: super: {
    lens = dontCheck super.lens;
    servant = dontCheck super.servant;
    servant-client = dontCheck super.servant-client;
    servant-server = dontCheck super.servant-server;
    mkDerivation = expr: super.mkDerivation (expr // { enableLibraryProfiling = true; });
  };
}
