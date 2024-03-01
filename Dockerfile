FROM haskell:9.6.4-slim


WORKDIR /project 

COPY . /project

RUN stack install alex happy

# Build your project
RUN stack build 



EXPOSE 3000 



CMD ["stack", "run"]