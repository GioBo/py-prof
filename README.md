# py-prof

## Summary

Easily observe python cProfile output in Emacs via ctable.


## Why use py-prof?

  The minor mode *py-prof* was created to make the use of cProfile easier (and more interactive) for Emacs users.
	

## Usage


  *py-prof* provides just 

  Just use
  
	M-x py-prof
	

  and you'll be asked to write down the function you want to profile, e.g.:
  
  
	my_long_function()
	
  
  and a buffer containing the results of the profiler output will be opened.
  
  The table has the following properties:
  
  - a column can be sorted by clicking on its header,
  - hit `RET` on a row and `py-prof` will try to open the file the line is referring to (this feature is still *experimental*, it is not guaranteed to work always). 
  
    


## Installing

You will need Emacs 24+, `make` and [Cask](https://github.com/cask/cask) to
build the project.

    cd py-prof
    make && make install


## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2017 boccigionata.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
