commit fdaa9eb8353543c7448dde68f712073e814fd519
Author: Bradley Phillips <bradder555@users.noreply.github.com>
Date:   Sun Mar 17 22:40:01 2019 +0800

    milestone - reads and writes digital IO
    
    I did not like the available modbus libraries, i figured 'how hard can
    it be?' so i've decided to write it using fsharp and dotnet core
    
    This has been developed on linux, although it should also run on
    windows (test someone?)
    
    Reading and writing of digital inputs and outputs is now successful
    
    The utility functions have ~100% test coverage - all of this is fairly
    trivial, but it gets confusing fast! better to test, so behaviour is
    documented also
    
    I think the digital IO is fairly robust, although i'd like to include
    more testing, reading and writing the registers is still untested (and
    probably incorrect)
    
    So far it's not a proper library, but it wouldn't be hard for me to
    create a library project, which can be imported by the example program
    (which includes the tests)
    
    It can get a bit confusing, getting one's head around byte orders,
    what i have adopted in my code is, the head of the list should be the
    least-significant, i.e. a uint of 258 should be converted to [2uy; 1uy]
    
    keeping 'my-side' in this format makes testing and understanding a lot
    easier
    
    I just need to remember to re-organise when serializing the modbus
    responses
    
    - develop and test the reading and writing of registers
    - handle out-of-bounds exceptions
    - develop and test the handling of receiving 'junk'
    - handle any other modbus-spec exceptions
    - convert to a library with examples
    - write a client interface
    - add tests for
      - serialization,
      - deserialization,
      - and exception handling
