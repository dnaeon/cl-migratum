FROM clfoundation/sbcl:2.2.1

ENV QUICKLISP_ADD_TO_INIT_FILE=true
ENV QUICKLISP_DIST_VERSION=latest

WORKDIR /root/quicklisp/local-projects/cl-migratum
COPY . .

RUN apt-get update && \
    apt-get install -y libsqlite3-dev && \
    rm -rf /var/lib/apt/lists/* && \
    /usr/local/bin/install-quicklisp

ENTRYPOINT ["./entrypoint.sh"]
