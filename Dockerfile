FROM haskell:8.2.1

RUN stack upgrade

ADD . /

CMD ["stack", "build"]
