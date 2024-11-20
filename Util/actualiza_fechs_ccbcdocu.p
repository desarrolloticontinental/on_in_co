
DEFINE VARIABLE nbr AS CHARACTER.
DEFINE VARIABLE nbr1 AS CHARACTER.
DEFINE VARIABLE cod AS CHARACTER.
DEFINE VARIABLE fch AS DATE.

cod  = "N/C".
nbr  = "001013911".
nbr1 = "001013916".
fch  = 03/31/08.

for each ccbcdocu where
    codcia = 1 and
    coddoc = cod and
    nrodoc >= nbr and
    nrodoc <= nbr1:
    ccbcdocu.fchdoc = fch.
    display
        coddoc
        nrodoc
        fchdoc
        .
    for each ccbddocu of ccbcdocu:
        ccbddocu.fchdoc = ccbcdocu.fchdoc.
        display CcbDDocu.codmat ccbddocu.fchdoc.
   end.
end.
