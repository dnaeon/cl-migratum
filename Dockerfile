FROM fukamachi/sbcl:latest

RUN apt-get update && apt-get install -y libsqlite3-dev

WORKDIR /root/.roswell/local-projects
COPY . .

ENTRYPOINT ["./entrypoint.sh"]
