let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "0937a03b510867336490dd835c336374454ecf5c";
      sha256 = "0g3gbjsx8nw3lki7zzypqn6bww87xc77kbl7k4a37ijpa6g62w7v";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

