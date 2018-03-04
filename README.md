# escprofiler
Command line escript tracing tool

## Todo

- Finish the script
- Test the script
  - Split the `test.erl` file down into seperate test functions that can make sure functionality is kept
    - remote function calls
    - same file function calls
    - anon function calls
    - Return values are being kept
    - Timer information is printed out
- Change the timer information so that it is only printed out after the file has finished running.
  - Spawn a process which receives time information
  - Build a list of the time information
  - Sort the list of times so that the longest function call is printed last
  - Print all the times
- ~~Add the function call data to the print out information - this is probably
one of the more important tasks to do~~
- Add timings for anon funs as well
