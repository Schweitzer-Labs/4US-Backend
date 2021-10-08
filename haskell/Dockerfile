ARG OUTPUT_DIR=/root/output
ARG EXECUTABLE_NAME=bootstrap

FROM lambci/lambda:build-provided as build

COPY . .

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

USER root

# Installing Haskell Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Build the lambda
COPY . /root/lambda-function/

RUN pwd

RUN cd /root/lambda-function
WORKDIR /root/lambda-function/

RUN ls

RUN stack clean --full
RUN stack build

ARG OUTPUT_DIR

RUN mkdir -p ${OUTPUT_DIR} && \
    mkdir -p ${OUTPUT_DIR}/lib

ARG EXECUTABLE_NAME

RUN cp $(stack path --local-install-root)/bin/${EXECUTABLE_NAME} ${OUTPUT_DIR}/${EXECUTABLE_NAME}

ENTRYPOINT sh

FROM public.ecr.aws/lambda/provided:al2 as deploy

ARG EXECUTABLE_NAME

WORKDIR ${LAMBDA_RUNTIME_DIR}

ARG OUTPUT_DIR

COPY --from=build ${OUTPUT_DIR} .

RUN ls
RUN mv ${EXECUTABLE_NAME} bootstrap || true
RUN ls

CMD [ "handler" ]
