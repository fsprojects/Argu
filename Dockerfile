FROM eiriktsarpalis/dotnet-sdk-mono:2.2.204-stretch

WORKDIR /app
COPY . .

CMD ./build.sh Bundle