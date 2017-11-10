let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "822212aa6c995bf58a8362c411f157227092c42d";
      sha256 = "1asqjh5r8kjvil1h7pggv408wamws8hnrdsm7a01nkvcm18gagad";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

