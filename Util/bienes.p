output to c:\tmp\bienes.txt.

for each lg-serv no-lock where coddoc = 'oca':
    display 
        LG-SERV.codser LG-SERV.desser with stream-io.
end.

output close.
