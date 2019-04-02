# Introduction
I did not like the available modbus libraries, i figured 'how hard can
it be?' so i've decided to write it using fsharp and dotnet core

This has been developed on x86_64 Ubuntu, it should run on anything with 
a NIC and can run the dotnetcore 2.2 run time (so ARM, x86), (Windows, Raspbian..)

## Project Status
The following has been implemented and is working
- FC 1, Read Coils 
- FC 2, Read Inputs
- FC 3, Read Holding Registers 
- FC 5, Write a Single Coil
- FC 6, Write a Single Holding Register 
- FC 15, Write multiple coils
- FC 16, Write multiple holding registers 

FC 4 has not been confirmed yet, once the deserialization testing has been
added, i will be confident that this component is working.

Failure to parse any of the function codes is handled and an error will be
responded, however this is generic and will need more work 

## Testing Status
A heap of utility functions were written to assist with all of the bit,
byte and word manipulations. 

The utility functions have ~100% test coverage - all of this is fairly
trivial, but it gets confusing fast! better to test, so behaviour is
documented also

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
  - deserialization,
  - and exception handling
- add round-trip testing
- add integration testing
