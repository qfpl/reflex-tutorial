let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "b365d0b3f55ebcca0c6f8517b20cf5d0b7b1cd37";
      sha256 = "198vckj8lls5d6ig4ayizk4mw7lsnbnynf187agagk2yfab0v6ll";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

