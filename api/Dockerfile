FROM debian

RUN apt-get update \
    && apt-get install -y \
       libssl1.0.0 \
       --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir /app
WORKDIR /app
COPY ./_rel /app

EXPOSE 8080
CMD ["/app/uc/bin/uc", "foreground"]
