The distribution contains generated data. If you want to contribute
please hack your way directly via the source repository.

For developing, you will need to install [uucd][1] and download a copy
of the XML Unicode character database to an absolute $DBPATH. From the
root directory of the repository type:

    ln -s $DBPATH support/ucd.xml
    ./build support

The result are in files `src/data/uucp_*_data.ml`. They contain data
extracted from the Unicode character database. These files are ignored 
by git. 

[1] http://erratique.ch/software/uucd
