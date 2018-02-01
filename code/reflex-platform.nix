let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "4c0a54b08f81a24958a4bbe42508244d4d74f3d1";
      sha256 = "0fcapmprzfl8q6y2bqb7qwmkwb13z4sdq12hzdmjzk2lzk4h099s";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

