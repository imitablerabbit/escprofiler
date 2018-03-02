# escprofiler
Command line escript tracing tool

## Todo

- Finish the script
- Test the script
- Change the wrapping function so that it will take in a map
	- The map will contain data that needs to be replaced in the wrapping
	function
	- This should stop the function from generating the abs form of the
	wrapping function each time a wrapping occurs
- Add the function call data to the print out information - this is probably
one of the more important tasks to do
- Add timings for anon funs as well
- Looks like `erl_parse` might not be the best way of doing this with escripts
as it has difficulty working with annotations in the code and general file
syntax rules. maybe `epp:parse_file` would be helpful
