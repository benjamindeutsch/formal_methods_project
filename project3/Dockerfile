FROM ubuntu:questing-20251007

SHELL ["/bin/bash", "-c"]

RUN apt-get -y update && apt-get -y upgrade
RUN apt-get install -y --no-install-recommends ca-certificates python3 git curl wget unzip build-essential libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config
RUN update-ca-certificates

RUN <<EOF
    cd $(mktemp -d)
    wget https://github.com/Z3Prover/z3/releases/download/z3-4.15.4/z3-4.15.4-x64-glibc-2.39.zip
    unzip z3-4.15.4-x64-glibc-2.39.zip
    mv z3-4.15.4-x64-glibc-2.39/bin/z3 /bin
EOF

RUN <<EOF
    wget -O /bin/ghcup https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup
    chmod +x /bin/ghcup
EOF

RUN ghcup install ghc 9.12.2 --force 
RUN ghcup set ghc 9.12.2
RUN ghcup install cabal 3.16.0.0 --force 
RUN ghcup set cabal 3.16.0.0

ENV PATH="$PATH:/root/.ghcup/bin"

RUN git clone --recurse-submodules -j8 https://github.com/ucsd-progsys/liquidhaskell

RUN <<EOF
    cd liquidhaskell
    git checkout --recurse-submodules f6c97e672d533802df0f634f6f89c664f477fa63
    cabal update 'hackage.haskell.org,2025-12-02T08:56:08Z'
    cabal build -j8 liquidhaskell
EOF

RUN <<EOF
    printf '#!/bin/bash\ncd /FMI\ncabal exec --project-dir=/liquidhaskell ghc -- -fplugin=LiquidHaskell -odir /build -hidir /build $@' > /usr/bin/liquid
    chmod +x /usr/bin/liquid
EOF

RUN <<EOF
    wget -O /usr/bin/py_factorial_test https://gist.githubusercontent.com/hetzenmat/eb663e2a5ecca76b25f092caaffe2621/raw/68a313bd87450278cbdfce714cd8ec5ea89af62f/gistfile1.txt
    chmod +x /usr/bin/py_factorial_test

    wget -O /FactorialTester.hs https://gist.githubusercontent.com/hetzenmat/b3a55043fdc2d2caa2bcffb5dc823815/raw/2d3ff9235a5047dffc6563295c0aaf6462fcef52/gistfile1.txt

    printf '#!/bin/bash\ncd $(mktemp -d)\n cp /FMI/Factorial.hs .\ncp /FactorialTester.hs .\npy_factorial_test' > /usr/bin/factorial_test
    chmod +x /usr/bin/factorial_test
EOF
