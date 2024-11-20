output to c:\tmp\servicios.txt.

for each lg-serv no-lock where coddoc = 'o/s':
    display 
        LG-SERV.codser LG-SERV.desser with stream-io.
end.

output close.
