FROM eiriktsarpalis/dotnet-sdk-mono:3.0.100-buster

WORKDIR /app
COPY . .

CMD ./build.sh -t Bundle