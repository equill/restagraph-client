with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "avoid-list";

    buildInputs = [
        # General utilities
        #pkgs.bash
        # Lisp env
        #pkgs.libyaml
        #pkgs.openssl
        pkgs.sbcl_2_1_9
        # Python env
        #pkgs.python39Full
        #pkgs.python39Packages.pip
        #pkgs.python39Packages.virtualenv
    ];

    env = buildEnv {
        name = name;
        paths = buildInputs;
    };

    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
        pkgs.openssl
        #pkgs.libyaml
    ];

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix avoid-list] \\w\\$\\[\\033[00m\\] '";

}
