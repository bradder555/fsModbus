# Introduction
.Net Core Modbus library written in FSharp

There's an example program with a few options

There's a dockerfile for running the example server

# Quickstart
The quick start section is broken into a 'Source' and a 'Docker' section

Read the source section if you're working with the library or playing with the example project

## Source
The interface is fairly straight forward refer to the example program to see how to use this library

The example program can be run with the following flags which will alter which components are run:
- --run-server: this runs the server component
- --run-client: this runs the client component
- --run-randomizer: this runs the randomizer component (dummy data presented by the server for read-only function codes)
- --binding=tcp://0.0.0.0:5502: the tcp port and interface you want to bind to (or connect to for the client)

## Docker 
The docker image is of the example project. As mentioned in the source section, there are a number of switches for various behaviour.

The docker image is configured to use the server and randomizer components only.

Thus to get going use something like

`
docker run -d --name myrandmodserver -p 502:5502 bradleyp81docker/modbus_tcp_test_server:latest
`

If you do not want to run the randomizer, you could use something like:
`
docker run -d --name mymodserver -p 502:5502 bradleyp81docker/modbus_tcp_test_server:latest dotnet /app/FsModbus.Example.dll --run-server --binding=tcp://0.0.0.0:5502
`

## Project Status
### Server
The following has been implemented and is working
- FC 1, Read Coils 
- FC 2, Read Inputs
- FC 3, Read Holding Registers 
- FC 5, Write a Single Coil
- FC 6, Write a Single Holding Register 
- FC 15, Write multiple coils
- FC 16, Write multiple holding registers 

### Client
- FC 1, Tested and confirmed 
- FC 6, Tested and confirmed
- FC 2, Tested and confirmed
- FC 3, Tested and confirmed
- FC 5, Tested and confirmed
- FC 15, Tested and confirmed
- FC 16, Tested and confirmed 


## Testing Status
A heap of utility functions were written to assist with all of the bit,
byte and word manipulations. 

The utility functions have ~100% test coverage - all of this is fairly
trivial, but it gets confusing fast! better to test, so behaviour is
documented also

### Server
The server had almost 100% test coverage, at one point, a major change was made that broke the tests.
It was not a major loss, since the functionality had been tested prior, and the change made wouldn't have changed this.
I'd like to restore this in the future for the sake of completeness

### Client
The client is tested against the server for complete round-trip testing.

A nice feature of the code, is the user of the server has two write their own delegate actions for each of the function codes. Consequently, when initializing the server we provide our own functions, this means that we have access to the mutable store that is used by the server.

## Philosophy / Design decisions
It can get a bit confusing, getting one's head around byte orders,
what i have adopted in my code is, the head of the list should be the
least-significant, i.e. a uint of 258 should be converted to [2uy; 1uy]

What is important is making the decision and sticking with it

## Todo:
- handle out-of-bounds and other deserialization exceptions
- develop and test the handling of receiving 'junk' (leads on from the first point)
- handle any other modbus-spec exceptions
- write a client interface
- add tests for
  - and exception handling
- get the testing working again (remove hopac etc)
