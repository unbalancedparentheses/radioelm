FROM alpine

RUN apk add --update \
    musl \
    python3 \
    python3-dev \
    build-base \
    postgresql-dev \
  && easy_install-3.4 pip \
  && rm -rf /var/cache/apk/*

RUN mkdir -p /app
WORKDIR /app

COPY requirements.txt /app/
RUN pip install --no-cache-dir -r requirements.txt

COPY . /app

EXPOSE 8000

VOLUME ["/app/static"]

CMD ["gunicorn", "radios_admin.wsgi", "--config=gunicorn.py", "-b", "0.0.0.0:8000"]
