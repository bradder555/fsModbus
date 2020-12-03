FROM mcr.microsoft.com/dotnet/sdk:3.1.404-alpine3.12 as builder 

WORKDIR /tmp/

RUN apk add git 
ADD https://api.github.com/repos/bradder555/fsModbus/commits/main version.json
RUN git clone https://github.com/bradder555/fsModbus.git /tmp/fsmodbus

WORKDIR /tmp/fsmodbus

RUN git checkout fe30862c06aff -b nowarn

RUN dotnet publish ./Example/FsModbus.Example.fsproj -c Release --self-contained=false

FROM mcr.microsoft.com/dotnet/runtime:3.1.10-alpine3.12 as target 
COPY --from=builder /tmp/fsmodbus/Example/bin/Release/netcoreapp3.1/publish /app
WORKDIR /app
RUN chmod +x FsModbus.Example
CMD ["dotnet", \
     "/app/FsModbus.Example.dll", \
     "--run-server", \
     "--run-randomizer", \
     "--binding=tcp://0.0.0.0:5502"]
