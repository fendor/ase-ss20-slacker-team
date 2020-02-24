
let
  pkgs = import <nixpkgs> {};
  buildInputs = with pkgs; [
    gmp
    zlib
    ncurses

    haskellPackages.cabal-install
    postgresql
  ];

  haskellShellHook = with pkgs; ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';

  postgresShellHook = ''
    ## Setup Postgres db
    export PGDATA=$PWD/postgres_data
    export PGHOST=$PWD/postgres
    export LOG_PATH=$PWD/postgres/LOG
    export PGDATABASE=postgres
    export DATABASE_URL="postgresql:///postgres?host=$PGHOST"
    if [ ! -d $PGHOST ]; then
      mkdir -p $PGHOST
    fi
    if [ ! -d $PGDATA ]; then
      echo 'Initializing postgresql database...'
      initdb --auth=trust >/dev/null
    fi
    pg_ctl start -l $LOG_PATH -o "-p 5432 -c listen_addresses=127.0.0.1 -c unix_socket_directories=$PGHOST"
  '';
in
pkgs.stdenv.mkDerivation {
  inherit buildInputs;
  name = "ase-devenv";

  shellHook = ''
    ${haskellShellHook}
    ${postgresShellHook}
    '';
}