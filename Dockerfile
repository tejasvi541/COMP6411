# trivial dockerfile to create Programming Language environment

FROM gcc

RUN apt-get update && apt-get install -y \
    bash-completion   \
    cmake             \
    valgrind          \
    nano              \
    scons             \
    clojure           \
    leiningen         \
    erlang            \
    rebar             \  
    && rm -rf /var/lib/apt/lists/*

