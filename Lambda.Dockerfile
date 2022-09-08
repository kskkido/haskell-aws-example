FROM amazonlinux:1
ARG STACK_RESOLVER=lts-19.22
ARG INPUT_DIR=/root/lambda-function
ARG OUTPUT_DIR=/root/output
ARG EXECUTABLE
SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]
USER root
RUN du -a /lib64 /usr/lib64 | cut -f2 > /root/default-libraries
RUN yum install -y \
    git-core \
    tar \
    sudo \
    xz \
    make \
    gmp-devel \
    libicu libicu-devel \
    libyaml libyaml-devel \
    ncurses-devel \
    postgresql-devel \
    zlib zlib-devel
RUN yum groupinstall -y "Development Tools" "Development Libraries"
RUN sudo curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup --resolver=${STACK_RESOLVER}
RUN mkdir ${INPUT_DIR} && \
    mkdir ${OUTPUT_DIR} && \
    mkdir ${OUTPUT_DIR}/lib
COPY . ${INPUT_DIR}/
RUN cd ${INPUT_DIR}
WORKDIR ${INPUT_DIR}/
RUN stack install record-dot-preprocessor
RUN stack build --ghc-options=-O0
RUN export LC_ALL=C.UTF-8
RUN cp $(stack path --local-install-root)/bin/${EXECUTABLE} ${OUTPUT_DIR}/bootstrap
RUN stack exec -- eriko-ikeda-portfolio-fp-cli-exe lambda-packager -i ${OUTPUT_DIR}/bootstrap -o ${OUTPUT_DIR}/lib
