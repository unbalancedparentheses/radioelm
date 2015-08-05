FROM nginx

COPY ./nginx/default.conf /etc/nginx/conf.d/default.conf

RUN mkdir /frontend
COPY ./frontend/build /frontend
