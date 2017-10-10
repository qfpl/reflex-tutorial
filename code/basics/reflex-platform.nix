let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "17f3d0b802faccb6542b0d98412f5084cca152f0";
      sha256 = "1yqf82b18nnkqm8wac7fw8wkh96rdgd8pdb72y9jwa7s8sxkkrh5";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

