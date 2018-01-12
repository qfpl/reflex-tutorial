let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "6aa1820d8d9ecae6744cd3f23f948a40a34f49c8";
      sha256 = "1b9zz9kxfmrva1jz7i6mn7kgmlx5d13aqx0izsw296bhhcf348g7";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

